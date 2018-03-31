using System;
using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace DeclareLocalVariable {
    internal static class LocalDeclarationStatementFactory {
        public static LocalDeclarationStatementSyntax CreateWithVar(String localVariableName, InvocationExpressionSyntax invocationExpression) =>
            Create("var", localVariableName, invocationExpression);

        public static LocalDeclarationStatementSyntax Create(String typeName, String localVariableName, InvocationExpressionSyntax invocationExpression) {
            var variableDeclarator = SyntaxFactory.VariableDeclarator(SyntaxFactory.Identifier(localVariableName),
                null,
                SyntaxFactory.EqualsValueClause(invocationExpression.WithoutTrivia()));
            var variableDeclaration = SyntaxFactory.VariableDeclaration(SyntaxFactory.IdentifierName(typeName),
                SyntaxFactory.SeparatedList(new List<VariableDeclaratorSyntax>() { variableDeclarator }));
            return SyntaxFactory.LocalDeclarationStatement(variableDeclaration);
        }

        public static LocalDeclarationStatementSyntax CreateLocalObjectStatement(String leftTypeName, String typeName, String localVariableName) {
            var objectCreationExpression = SyntaxFactory.ObjectCreationExpression(SyntaxFactory.IdentifierName(typeName),
                typeName.EndsWith("[]") ? null : SyntaxFactory.ArgumentList(),
                null);
            var variableDeclarator = SyntaxFactory.VariableDeclarator(SyntaxFactory.Identifier(localVariableName),
                null,
                SyntaxFactory.EqualsValueClause(objectCreationExpression));
            var variableDeclaration = SyntaxFactory.VariableDeclaration(SyntaxFactory.IdentifierName(leftTypeName),
                SyntaxFactory.SeparatedList(new List<VariableDeclaratorSyntax>() { variableDeclarator }));
            return SyntaxFactory.LocalDeclarationStatement(variableDeclaration);
        }
    }
}
