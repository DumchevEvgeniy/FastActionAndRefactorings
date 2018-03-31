using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace DeclareLocalVariable.DefaultObjectCreateRefactoring {
    public abstract class DefaultObjectCodeRefactoring {
        public static async Task<Solution> CreateDefaultObject(CodeRefactoringContext context, Boolean leftTypeNameIsVar) {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
            var currentNode = root.FindNode(context.Span);
            var semanticModel = await context.Document.GetSemanticModelAsync(context.CancellationToken);
            var statement = currentNode.AncestorsAndSelf().First(n => n is ExpressionStatementSyntax ||
                n is LocalDeclarationStatementSyntax) as StatementSyntax;
            var type = GetTypeInfo(statement, semanticModel);
            var localVariableName = LocalVariableNameGenerator.Create(GetNameForLocalVariable(type), currentNode, semanticModel);
            var typeName = statement.WithoutTrivia().GetText().ToString().Replace(" ", "").TrimEnd(';');
            var leftTypeName = leftTypeNameIsVar ? "var" : typeName;
            var localDeclarationStatement = LocalDeclarationStatementFactory.CreateLocalObjectStatement(leftTypeName, typeName, localVariableName);
            return await SolutionNodeReplacer.Replace(context, localDeclarationStatement, statement);
        }

        public static async Task<Boolean> IsCallPoint(CodeRefactoringContext context) {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
            var node = root.FindNode(context.Span);
            var statement = node.AncestorsAndSelf().FirstOrDefault(n => n is ExpressionStatementSyntax ||
                n is LocalDeclarationStatementSyntax) as StatementSyntax;
            if (statement == null)
                return false;
            var semanticModel = await context.Document.GetSemanticModelAsync(context.CancellationToken);
            if (semanticModel == null)
                return false;
            if (!GeneralTypeInStatementIsValid(statement, semanticModel))
                return false;
            if (!AllTypeInStatementIsValid(statement, semanticModel))
                return false;
            return true;
        }

        private static Boolean EnumerableIsEmpty<T>(IEnumerable<T> enumerable) => enumerable != null && !enumerable.Any();
        private static Boolean AllTypeInStatementIsValid(StatementSyntax statement, SemanticModel semanticModel) {
            var listNodes = statement.DescendantNodes().ToList();
            var member = listNodes.OfType<MemberAccessExpressionSyntax>().FirstOrDefault();
            var typeList = member != null ? member.Name.DescendantNodesAndSelf().OfType<TypeSyntax>() : listNodes.OfType<TypeSyntax>();
            if (!AllTypeIsExist(typeList.ToList(), semanticModel))
                return false;
            if (typeList.First().GetLastToken().GetNextToken().Kind() != SyntaxKind.SemicolonToken)
                return false;
            var beginTokenInType = member != null ? member.GetFirstToken() : typeList.First().GetFirstToken();
            if (statement.DescendantNodesAndTokens().Contains(beginTokenInType.GetPreviousToken()))
                return false;
            return true;
        }
        private static Boolean AllTypeIsExist(List<TypeSyntax> typeList, SemanticModel semanticModel) {
            if (EnumerableIsEmpty(typeList))
                return false;
            return !typeList.Exists(type => !TypeIsPermissible(semanticModel.GetTypeInfo(type).Type));
        }
        private static Boolean GeneralTypeInStatementIsValid(StatementSyntax statement, SemanticModel semanticModel) {
            var type = GetTypeInfo(statement, semanticModel);
            var currentClass = statement.Ancestors().OfType<ClassDeclarationSyntax>().First();
            var declaredSymbol = semanticModel.GetDeclaredSymbol(currentClass);
            if (!HasVisibleConstructor(type, declaredSymbol))
                return false;
            return true;
        }
        private static Boolean HasVisibleConstructor(ITypeSymbol type, INamedTypeSymbol currentClass) {
            if (currentClass == null)
                return false;
            if (!TypeIsPermissible(type))
                return false;
            if (type is IArrayTypeSymbol)
                return true;
            if (type.IsAbstract)
                return false;
            var members = type.GetMembers();
            if (EnumerableIsEmpty(members))
                return false;
            var constructors = members.Where(m => (m.Kind == SymbolKind.Method) &&
                (m as IMethodSymbol).MethodKind == MethodKind.Constructor).Select(m => m as IMethodSymbol).ToList();
            if (EnumerableIsEmpty(constructors))
                return false;
            if (constructors.Exists(c => c.DeclaredAccessibility == Accessibility.ProtectedOrInternal ||
                                         c.DeclaredAccessibility == Accessibility.Public ||
                                         c.DeclaredAccessibility == Accessibility.Internal))
                return true;
            if (constructors.Exists(c => (c.DeclaredAccessibility == Accessibility.Private ||
                                         c.DeclaredAccessibility == Accessibility.Protected) &&
                                         type.Name == currentClass.Name))
                return true;
            return false;
        }
        private static Boolean TypeIsPermissible(ITypeSymbol type) {
            if (type == null || !type.IsType || type is IErrorTypeSymbol)
                return false;
            if (type.IsStatic)
                return false;
            if (type is INamedTypeSymbol namedType && namedType.TypeArguments.ToList().Exists(t => t == null || t is IErrorTypeSymbol))
                return false;
            return true;
        }

        private static String GetNameForLocalVariable(ITypeSymbol type) {
            var arrayType = type as IArrayTypeSymbol;
            var endResultName = arrayType != null ? "Array" : string.Empty;
            var name = arrayType != null ? arrayType.ElementType.Name : type.Name;
            var nameWithLower = name[0].ToString().ToLower() + name.Remove(0, 1);
            return nameWithLower + endResultName;
        }

        private static ITypeSymbol GetTypeInfo(StatementSyntax statement, SemanticModel semanticModel) {
            var expression = statement as ExpressionStatementSyntax;
            var local = statement as LocalDeclarationStatementSyntax;
            if (local == null && expression == null)
                return null;
            var typeInfo = expression != null ?
                semanticModel.GetTypeInfo(expression.Expression) :
                semanticModel.GetTypeInfo(local.Declaration.Type);
            return typeInfo.Type;
        }
    }
}