using System;
using System.Composition;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeRefactorings;
using System.Linq;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CodeActions;
using System.Collections.Generic;

namespace DeclareLocalVariable.DeclareClassRefactoring {
    [ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = nameof(DeclareClassCodeRefactoringProvider)), Shared]
    public class DeclareClassCodeRefactoringProvider : CodeRefactoringProvider {
        private readonly String refactoringName = "Declare class";
        private String nameExpectedCreatedClass;

        public sealed override async Task ComputeRefactoringsAsync(CodeRefactoringContext context) {
            var isCallPoint = await IsCallPoint(context);
            if (isCallPoint) {
                var action = CodeAction.Create(refactoringName, c => DeclareClass(context));
                context.RegisterRefactoring(action);
            }
        }

        private async Task<Boolean> IsCallPoint(CodeRefactoringContext context) {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
            var currentNode = root.FindNode(context.Span);
            var statement = currentNode.AncestorsAndSelf().FirstOrDefault(n => n is StatementSyntax) as StatementSyntax;
            if (statement == null)
                return false;
            var block = statement.Ancestors().FirstOrDefault(n => n is BlockSyntax) as BlockSyntax;
            if (block == null)
                return false;
            var semanticModel = await context.Document.GetSemanticModelAsync(context.CancellationToken);
            nameExpectedCreatedClass = GetNameLocalVaraibleExpectedCreatedClass(semanticModel, block)
                ?.FirstOrDefault(name => ExistLocalVariable(statement, name));
            return nameExpectedCreatedClass != null;
        }

        private async Task<Document> DeclareClass(CodeRefactoringContext context) {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
            var currentNode = root.FindNode(context.Span);
            var block = currentNode.Ancestors().OfType<BlockSyntax>().FirstOrDefault();
            var semanticModel = await context.Document.GetSemanticModelAsync(context.CancellationToken);
            Boolean classDefenitionIsFound = false;
            ClassDeclarationSyntax classDeclaration = null;
            var memberDeclarations = new List<MemberDeclarationSyntax>();
            foreach (var statement in block.DescendantNodes().OfType<StatementSyntax>()) {
                if (!classDefenitionIsFound && statement is LocalDeclarationStatementSyntax && ExistLocalVariable(statement, nameExpectedCreatedClass)) {
                    var className = GetClassName(statement as LocalDeclarationStatementSyntax);
                    classDeclaration = SyntaxFactory.ClassDeclaration(className);
                    classDefenitionIsFound = true;
                }
                if (!classDefenitionIsFound)
                    continue;

                var memberDeclaration = statement.Accept(new StatementVisitor(nameExpectedCreatedClass, semanticModel));
                if (memberDeclaration != null)
                    memberDeclarations.Add(memberDeclaration);
            }
            classDeclaration = CreateClass(classDeclaration, memberDeclarations);

            return await AddNewClass(context, classDeclaration);
        }
        public async Task<Document> AddNewClass(CodeRefactoringContext context, ClassDeclarationSyntax newClassDeclaration) {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
            var currentNode = root.FindNode(context.Span);
            var classDeclaration = currentNode.AncestorsAndSelf().OfType<ClassDeclarationSyntax>().FirstOrDefault();
            var newSyntaxRoot = root.InsertNodesAfter(classDeclaration, new List<SyntaxNode> { newClassDeclaration });
            var newRoot = root.ReplaceNode(root, newSyntaxRoot);
            return context.Document.WithSyntaxRoot(newRoot);
        }

        public ClassDeclarationSyntax CreateClass(ClassDeclarationSyntax classDeclaration, IEnumerable<MemberDeclarationSyntax> members) {
            if (members == null || !members.Any())
                return classDeclaration;
            members = members.Distinct();

            var properties = members.Where(m => m is PropertyDeclarationSyntax).Select(m => m as PropertyDeclarationSyntax).OrderBy(m => m.Identifier.ValueText);
            foreach (var property in properties)
                classDeclaration = classDeclaration.AddMembers(WithFormatter(property));

            var constructors = members.Where(m => m is ConstructorDeclarationSyntax).Select(m => m as ConstructorDeclarationSyntax).OrderBy(m => m.ParameterList.Parameters.Count);
            foreach (var constructor in constructors)
                classDeclaration = classDeclaration.AddMembers(WithFormatter(constructor));

            var methods = members.Where(m => m is MethodDeclarationSyntax)
                .Select(m => m as MethodDeclarationSyntax)
                .OrderBy(m => m.Identifier.ValueText)
                .ThenBy(m => m.ParameterList.Parameters.Count);
            foreach (var method in methods)
                classDeclaration = classDeclaration.AddMembers(WithFormatter(method));

            return classDeclaration;
        }

        private MemberDeclarationSyntax WithFormatter(MemberDeclarationSyntax member) {
            var endOfLineTrivia = SyntaxFactory.SyntaxTrivia(SyntaxKind.EndOfLineTrivia, String.Empty);
            return member.WithTrailingTrivia(new List<SyntaxTrivia> { endOfLineTrivia });
        }

        private String GetClassName(LocalDeclarationStatementSyntax localDeclarationStatement) {
            var objectCreation = localDeclarationStatement.DescendantNodes().OfType<ObjectCreationExpressionSyntax>().FirstOrDefault();
            return objectCreation.DescendantNodes().OfType<IdentifierNameSyntax>().FirstOrDefault().Identifier.ValueText;
        }

        private Boolean ExistLocalVariable(StatementSyntax statement, String localVaribleName) {
            return statement.DescendantNodesAndTokens().Any(n => {
                var token = n.AsToken();
                if (token == null)
                    return false;
                return token.Text == localVaribleName;
            });
        }
        public Boolean TypeIsExist(TypeInfo typeInfo) {
            var type = typeInfo.Type;
            if (type == null || !type.IsType || type is IErrorTypeSymbol)
                return false;
            var namedType = type as INamedTypeSymbol;
            if (namedType == null)
                return false;
            if (namedType.TypeArguments.ToList().Exists(t => t == null || t is IErrorTypeSymbol))
                return false;
            return true;
        }

        private IEnumerable<String> GetNameLocalVaraibleExpectedCreatedClass(SemanticModel semanticModel, BlockSyntax blockSyntax) {
            foreach (var node in blockSyntax.DescendantNodes()) {
                var localDeclarationStatement = node as LocalDeclarationStatementSyntax;
                if (localDeclarationStatement == null)
                    continue;
                if (localDeclarationStatement.Declaration == null)
                    continue;
                var variableDeclaration = localDeclarationStatement.Declaration as VariableDeclarationSyntax;
                if (variableDeclaration == null || !variableDeclaration.Variables.Any())
                    continue;
                var declarator = variableDeclaration.Variables.First();
                var objectCreation = declarator.DescendantNodes().FirstOrDefault(n => n is ObjectCreationExpressionSyntax) as ObjectCreationExpressionSyntax;
                if (objectCreation == null)
                    continue;
                var typeInfo = semanticModel.GetTypeInfo(objectCreation);
                if (TypeIsExist(typeInfo))
                    continue;
                yield return declarator.GetFirstToken().Text;
            }
        }

        private class StatementVisitor : CSharpSyntaxVisitor<MemberDeclarationSyntax> {
            private readonly String localVaribleName;
            private readonly SemanticModel semanticModel;

            public StatementVisitor(String localVaribleName, SemanticModel semanticModel) {
                this.localVaribleName = localVaribleName;
                this.semanticModel = semanticModel;
            }

            public override MemberDeclarationSyntax VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node) {
                if (!ExistLocalVariable(node))
                    return null;
                var variableDeclaration = node.DescendantNodes().OfType<VariableDeclarationSyntax>().FirstOrDefault();
                if (variableDeclaration == null)
                    return null;
                return variableDeclaration.Accept(new ExpressionVisitor(localVaribleName, semanticModel));
            }

            public override MemberDeclarationSyntax VisitExpressionStatement(ExpressionStatementSyntax node) {
                if (!ExistLocalVariable(node))
                    return null;
                return node.Expression.Accept(new ExpressionVisitor(localVaribleName, semanticModel));
            }


            private Boolean ExistLocalVariable(StatementSyntax statement) {
                return statement.DescendantNodesAndTokens().Any(n => {
                    var token = n.AsToken();
                    if (token == null)
                        return false;
                    return token.Text == localVaribleName;
                });
            }
        }

        private class ExpressionVisitor : CSharpSyntaxVisitor<MemberDeclarationSyntax> {
            private readonly String localVaribleName;
            private readonly SemanticModel semanticModel;

            public ExpressionVisitor(String localVaribleName, SemanticModel semanticModel) {
                this.localVaribleName = localVaribleName;
                this.semanticModel = semanticModel;
            }

            public override MemberDeclarationSyntax VisitAssignmentExpression(AssignmentExpressionSyntax node) {
                if (node.Left == null || node.Right == null || node.OperatorToken == null || node.OperatorToken.Kind() != SyntaxKind.EqualsToken)
                    return null;
                if (node.Left.Kind() != SyntaxKind.SimpleMemberAccessExpression)
                    return null;
                var simpleMemberAccessExpression = node.Left as MemberAccessExpressionSyntax;
                var left = simpleMemberAccessExpression.Expression as IdentifierNameSyntax;
                if (left == null || left.Identifier == null || left.Identifier.Text != localVaribleName)
                    return null;
                var typeInfo = semanticModel.GetTypeInfo(node.Right);
                return CodeElementFactory.CreateProperty(CodeElementFactory.CreateType(typeInfo), simpleMemberAccessExpression.Name.Identifier.Text);
            }

            public override MemberDeclarationSyntax VisitInvocationExpression(InvocationExpressionSyntax node) {
                if (node.Expression == null)
                    return null;
                if (node.Expression.Kind() != SyntaxKind.SimpleMemberAccessExpression)
                    return null;
                var simpleMemberAccessExpression = node.Expression as MemberAccessExpressionSyntax;
                var left = simpleMemberAccessExpression.Expression as IdentifierNameSyntax;
                if (left == null || left.Identifier == null || left.Identifier.Text != localVaribleName)
                    return null;
                return CodeElementFactory.CreareMethodDeclaration(CodeElementFactory.CreateType(SyntaxKind.VoidKeyword),
                    simpleMemberAccessExpression.Name.Identifier.Text,
                    GetParametersTypesByArguments(node.ArgumentList)
                );
            }

            public override MemberDeclarationSyntax VisitVariableDeclaration(VariableDeclarationSyntax node) {
                if (IsObjectCreation(node))
                    return CreateConstructor(node);
                if (IsInvocationExpression(node))
                    return CreateMethod(node);
                if (IsSimpleMemberAccessExpression(node))
                    return CreateProperty(node);
                return null;
            }

            private Boolean IsObjectCreation(VariableDeclarationSyntax node) {
                var variableDeclarator = node.DescendantNodes().OfType<VariableDeclaratorSyntax>().FirstOrDefault();
                if (variableDeclarator == null)
                    return false;
                if (variableDeclarator.GetFirstToken().ValueText != localVaribleName)
                    return false;
                return variableDeclarator.DescendantNodes().Any(n => n is ObjectCreationExpressionSyntax);
            }
            private Boolean IsInvocationExpression(VariableDeclarationSyntax node) {
                var invocationExpression = node.DescendantNodes().OfType<InvocationExpressionSyntax>().FirstOrDefault();
                if (invocationExpression == null)
                    return false;
                var identifier = invocationExpression.DescendantNodes().OfType<IdentifierNameSyntax>().FirstOrDefault();
                if (identifier == null || identifier.Identifier.ValueText != localVaribleName)
                    return false;
                return true;
            }
            private Boolean IsSimpleMemberAccessExpression(VariableDeclarationSyntax node) {
                var memberAccessExpression = node.DescendantNodes().OfType<MemberAccessExpressionSyntax>().FirstOrDefault();
                if (memberAccessExpression.DescendantNodes().Any(n => !(n is IdentifierNameSyntax)))
                    return false;
                var left = memberAccessExpression.DescendantNodes().OfType<IdentifierNameSyntax>().FirstOrDefault();
                if (left == null || left.Identifier.ValueText != localVaribleName)
                    return false;
                return true;
            }


            private MemberDeclarationSyntax CreateConstructor(VariableDeclarationSyntax node) {
                var objectCreationExpression = node.DescendantNodes().OfType<ObjectCreationExpressionSyntax>().FirstOrDefault();
                var className = objectCreationExpression.DescendantNodes().OfType<IdentifierNameSyntax>().FirstOrDefault().Identifier.ValueText;
                return CodeElementFactory.CreateConstructor(className, GetParametersTypesByArguments(objectCreationExpression.ArgumentList));
            }
            private MemberDeclarationSyntax CreateMethod(VariableDeclarationSyntax node) {
                var invocationExpression = node.DescendantNodes().OfType<InvocationExpressionSyntax>().FirstOrDefault();
                var methodName = invocationExpression.DescendantNodes().OfType<MemberAccessExpressionSyntax>().FirstOrDefault().GetLastToken().ValueText;
                var parameters = GetParametersTypesByArguments(invocationExpression.ArgumentList);
                var type = semanticModel.GetTypeInfo(node.DescendantNodes().OfType<IdentifierNameSyntax>().FirstOrDefault());
                return CodeElementFactory.CreareMethodDeclaration(CodeElementFactory.CreateType(type), methodName, parameters);
            }
            private MemberDeclarationSyntax CreateProperty(VariableDeclarationSyntax node) {
                var type = semanticModel.GetTypeInfo(node.DescendantNodes().OfType<IdentifierNameSyntax>().FirstOrDefault());
                var propertyName = node.DescendantNodes().OfType<IdentifierNameSyntax>().LastOrDefault().Identifier.ValueText;
                return CodeElementFactory.CreateProperty(CodeElementFactory.CreateType(type), propertyName);
            }

            private IEnumerable<TypeSyntax> GetParametersTypesByArguments(ArgumentListSyntax argumentList) {
                if (argumentList == null || !argumentList.Arguments.Any())
                    yield break;
                foreach (var argument in argumentList.Arguments) {
                    var typeInfo = semanticModel.GetTypeInfo(argument.Expression);
                    yield return CodeElementFactory.CreateType(typeInfo);
                }
            }
        }
    }
}
