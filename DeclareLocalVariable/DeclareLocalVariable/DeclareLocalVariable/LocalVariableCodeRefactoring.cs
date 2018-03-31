using System;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Rename;

namespace DeclareLocalVariable {
    public static class LocalVariableCodeRefactoring {
        public static async Task<Solution> DeclareLocalVariable(CodeRefactoringContext context, String typeName) {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
            var currentNode = root.FindNode(context.Span);
            var invocationExpression = currentNode.DescendantNodesAndSelf().OfType<InvocationExpressionSyntax>().First();
            var identifierName = invocationExpression.DescendantNodesAndSelf().LastOrDefault(n => n is IdentifierNameSyntax) as IdentifierNameSyntax;
            var methodName = MethodNameGenerator.CreateIdentifierName(identifierName.Identifier.ValueText);
            var localVariableName = LocalVariableNameGenerator.Create(methodName, currentNode, await context.Document.GetSemanticModelAsync());
            var localDeclarationStatement = LocalDeclarationStatementFactory.Create(typeName, localVariableName, invocationExpression);
            var expression = currentNode.DescendantNodesAndSelf().OfType<ExpressionStatementSyntax>().First();
            var localStatementWithTrivia = localDeclarationStatement
                .WithLeadingTrivia(expression.GetLeadingTrivia())
                .WithTrailingTrivia(expression.GetTrailingTrivia());
            var block = expression.AncestorsAndSelf().OfType<BlockSyntax>().First();
            var newRoot = root.ReplaceNode(block, block.ReplaceNode(expression, localStatementWithTrivia));
            var newDocument = context.Document.WithSyntaxRoot(newRoot);
            return await RenameVariableNewObject(newDocument, context);
        }

        private static async Task<Solution> RenameVariableNewObject(Document newDocument, CodeRefactoringContext context) {
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

        public static async Task<Boolean> IsCallPoint(CodeRefactoringContext context) {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
            var currentNode = root.FindNode(context.Span);
            var statement = currentNode.DescendantNodesAndSelf().OfType<ExpressionStatementSyntax>().FirstOrDefault();
            if (statement == null)
                return false;
            if (!currentNode.DescendantNodesAndSelf().Any(n => n is InvocationExpressionSyntax))
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
            if (namedType != null && namedType.TypeArguments.ToList().Exists(t => t == null || t is IErrorTypeSymbol))
                return null;
            return namedType;
        }
    }

}
