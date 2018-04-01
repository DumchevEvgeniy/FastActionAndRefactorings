using System;
using System.Composition;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace DeclareLocalVariable.DeclareLocalVariableRefactoring {
    [ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = nameof(DeclareLocalVariableWithVarCodeRefactoringProvider)), Shared]
    public class DeclareLocalVariableWithVarCodeRefactoringProvider : CodeRefactoringProvider {
        private readonly String refactoringName = "Declare local variable";

        protected async Task<String> GetTypeName(CodeRefactoringContext context) {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
            var currentNode = root.FindNode(context.Span);
            var semanticModel = await context.Document.GetSemanticModelAsync(context.CancellationToken);
            var statement = currentNode.AncestorsAndSelf().OfType<ExpressionStatementSyntax>().FirstOrDefault();
            return LocalVariableCodeRefactoring.GetNamedType(semanticModel, statement).MetadataName;
        }

        public sealed override async Task ComputeRefactoringsAsync(CodeRefactoringContext context) {
            var isCallPoint = await LocalVariableCodeRefactoring.IsCallPoint(context);
            if (!isCallPoint)
                return;
            var typeName = await GetTypeName(context);
            var action = CodeAction.Create(refactoringName, c => LocalVariableCodeRefactoring.DeclareLocalVariable(context, typeName));
            context.RegisterRefactoring(action);
        }
    }
}
