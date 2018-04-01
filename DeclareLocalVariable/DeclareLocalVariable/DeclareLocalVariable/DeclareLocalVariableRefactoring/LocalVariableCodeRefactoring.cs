using System;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace DeclareLocalVariable.DeclareLocalVariableRefactoring {
    public static class LocalVariableCodeRefactoring {
        public static async Task<Solution> DeclareLocalVariable(CodeRefactoringContext context, String typeName) {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
            var currentNode = root.FindNode(context.Span);
            var statement = currentNode.AncestorsAndSelf().OfType<ExpressionStatementSyntax>().FirstOrDefault();
            var invocationExpression = statement.DescendantNodesAndSelf().OfType<InvocationExpressionSyntax>().First();
            var identifierName = invocationExpression.DescendantNodesAndSelf().LastOrDefault(n => n is IdentifierNameSyntax) as IdentifierNameSyntax;
            var methodName = LocalNameByMathodNameGenerator.CreateIdentifierName(identifierName.Identifier.ValueText);
            var localVariableName = LocalVariableNameGenerator.Create(methodName, currentNode, await context.Document.GetSemanticModelAsync());
            var localDeclarationStatement = LocalDeclarationStatementFactory.Create(typeName, localVariableName, invocationExpression);

            return await SolutionNodeReplacer.Replace(context, localDeclarationStatement, statement);
        }

        public static async Task<Boolean> IsCallPoint(CodeRefactoringContext context) {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
            var currentNode = root.FindNode(context.Span);
            var statement = currentNode.AncestorsAndSelf().OfType<ExpressionStatementSyntax>().FirstOrDefault();
            if (statement == null)
                return false;
            if (!statement.DescendantNodesAndSelf().Any(n => n is InvocationExpressionSyntax))
                return false;
            var semanticModel = await context.Document.GetSemanticModelAsync(context.CancellationToken);
            return GetNamedType(semanticModel, statement) != null;
        }

        public static INamedTypeSymbol GetNamedType(SemanticModel semanticModel, ExpressionStatementSyntax statement) {
            if (semanticModel == null)
                return null;
            var type = semanticModel.GetTypeInfo(statement.Expression).Type;
            if (type == null || !type.IsType || type is IErrorTypeSymbol)
                return null;
            var namedType = type as INamedTypeSymbol;
            if (namedType == null)
                return null;
            if (namedType.SpecialType == SpecialType.System_Void)
                return null;
            if (namedType.TypeArguments.ToList().Exists(t => t == null || t is IErrorTypeSymbol || t.SpecialType == SpecialType.System_Void))
                return null;
            return namedType;
        }
    }

}
