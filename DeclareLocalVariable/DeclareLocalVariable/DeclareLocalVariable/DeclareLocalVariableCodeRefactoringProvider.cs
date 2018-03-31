using System;
using System.Collections.Generic;
using System.Composition;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Rename;
using Microsoft.CodeAnalysis.Text;

namespace DeclareLocalVariable {
    [ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = nameof(DeclareLocalVariableCodeRefactoringProvider)), Shared]
    internal class DeclareLocalVariableCodeRefactoringProvider : CodeRefactoringProvider {
        public sealed override async Task ComputeRefactoringsAsync(CodeRefactoringContext context) {
            var isCallPoint = await IsCallPoint(context);
            if (!isCallPoint)
                return;
            var action = CodeAction.Create("Declare local variable", c => DeclareLocalVariable(context));
            context.RegisterRefactoring(action);
        }

        private async Task<Solution> DeclareLocalVariable(CodeRefactoringContext context) {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
            var currentNode = root.FindNode(context.Span);
            var invocationExpression = currentNode.DescendantNodesAndSelf().OfType<InvocationExpressionSyntax>().First();
            var identifierName = invocationExpression.DescendantNodesAndSelf().LastOrDefault(n => n is IdentifierNameSyntax) as IdentifierNameSyntax;
            var methodName = MethodNameGenerator.CreateIdentifierName(identifierName.Identifier.ValueText);
            var localVariableName = LocalVariableNameGenerator.Create(methodName, currentNode, await context.Document.GetSemanticModelAsync());
            var localDeclarationStatement = LocalDeclarationStatementFactory.Create(localVariableName, invocationExpression);
            var expression = currentNode.DescendantNodesAndSelf().OfType<ExpressionStatementSyntax>().First();
            var localStatementWithTrivia = localDeclarationStatement
                .WithLeadingTrivia(expression.GetLeadingTrivia())
                .WithTrailingTrivia(expression.GetTrailingTrivia());
            var block = expression.AncestorsAndSelf().OfType<BlockSyntax>().First();
            var newRoot = root.ReplaceNode(block, block.ReplaceNode(expression, localStatementWithTrivia));
            var newDocument = context.Document.WithSyntaxRoot(newRoot);
            return await RenameVariableNewObject(newDocument, context);
        }

        private async Task<Solution> RenameVariableNewObject(Document newDocument, CodeRefactoringContext context) {
            var semanticModel = await newDocument.GetSemanticModelAsync();
            var newNodeRoot = await newDocument.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
            var newNode = newNodeRoot.FindNode(context.Span);
            var localDeclaration = newNode.AncestorsAndSelf().OfType<LocalDeclarationStatementSyntax>().First();
            var nameLocalVariable = semanticModel.GetDeclaredSymbol(localDeclaration.Declaration.Variables.First());
            return await Renamer.RenameSymbolAsync(newDocument.Project.Solution,
                nameLocalVariable,
                nameLocalVariable.Name,
                newDocument.Project.Solution.Workspace.Options);
        }

        private async Task<Boolean> IsCallPoint(CodeRefactoringContext context) {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
            var currentNode = root.FindNode(context.Span);
            var statement = currentNode.DescendantNodesAndSelf().OfType<ExpressionStatementSyntax>().FirstOrDefault();
            if (statement == null)
                return false;
            if (!currentNode.DescendantNodesAndSelf().Any(n => n is InvocationExpressionSyntax))
                return false;
            var semanticModel = await context.Document.GetSemanticModelAsync(context.CancellationToken);
            if (semanticModel == null)
                return false;
            var type = semanticModel.GetTypeInfo(statement.Expression).Type;
            if (type == null || !type.IsType || type is IErrorTypeSymbol)
                return false;
            if (type is INamedTypeSymbol namedType && namedType.TypeArguments.ToList().Exists(t => t == null || t is IErrorTypeSymbol))
                return false;
            return true;
        }  
    }

    internal static class LocalVariableNameGenerator {
        public static String Create(String name, SyntaxNode node, SemanticModel semanticModel) {
            var localVariables = GetNamesAllLocalVariables(node, semanticModel);
            return GetLocalVariableName(name, localVariables);
        }

        private static List<String> GetNamesAllLocalVariables(SyntaxNode node, SemanticModel semanticModel) {
            var resultList = new List<String>();
            var declaration = node.Ancestors().OfType<BaseMethodDeclarationSyntax>().First();
            var declarationInfo = semanticModel.GetDeclaredSymbol(declaration);
            var parameterNames = declarationInfo.Parameters.Select(p => p.Name);
            if (parameterNames != null)
                resultList.AddRange(parameterNames);
            foreach (var local in declaration.DescendantNodes().OfType<LocalDeclarationStatementSyntax>())
                foreach (var variable in local.Declaration.Variables)
                    resultList.Add(semanticModel.GetDeclaredSymbol(variable).Name);
            return resultList;
        }

        private static String GetLocalVariableName(String name, List<String> localVariables) {
            if (localVariables == null || localVariables.Count == 0)
                return name;
            var result = name;
            var number = 0;
            while (localVariables.Contains(result))
                result = name + (++number).ToString();
            return result;
        }
    }

    internal static class LocalDeclarationStatementFactory {
        public static LocalDeclarationStatementSyntax Create(string localVariableName, InvocationExpressionSyntax invocationExpression) {
            var variableDeclarator = SyntaxFactory.VariableDeclarator(SyntaxFactory.Identifier(localVariableName),
                null,
                SyntaxFactory.EqualsValueClause(invocationExpression.WithoutTrivia()));
            var variableDeclaration = SyntaxFactory.VariableDeclaration(SyntaxFactory.IdentifierName("var"),
                SyntaxFactory.SeparatedList(new List<VariableDeclaratorSyntax>() { variableDeclarator }));
            return SyntaxFactory.LocalDeclarationStatement(variableDeclaration);
        }
    }

    internal static class MethodNameGenerator {
        private static readonly IEnumerable<String> startsDefaultNames = new List<String> {
            "to", "try", "set", "create", "get", "with", "without"
        };

        public static String CreateIdentifierName(String methodName) {
            var newStr = GetName(methodName);
            return Char.ToLower(newStr.First()) + newStr.Remove(0, 1);
        }

        private static String GetName(String methodName) {
            foreach(var startsDefaultName in startsDefaultNames)
                if (StartsWith(methodName, startsDefaultName))
                    return methodName.Remove(0, startsDefaultName.Length);
            return methodName;
        }
        private static Boolean StartsWith(String methodName, String startsDefaultName) {
            if (methodName.Length <= startsDefaultName.Length)
                return false;
            for (Int32 i = 0; i < startsDefaultName.Length; i++)
                if (Char.ToLower(methodName[i]) != startsDefaultName[i])
                    return false;
            return true;
        }
    }
}
