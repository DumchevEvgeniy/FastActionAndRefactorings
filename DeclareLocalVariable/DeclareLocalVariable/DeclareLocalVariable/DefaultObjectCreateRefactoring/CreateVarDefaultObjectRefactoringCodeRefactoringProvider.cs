using System;
using System.Composition;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeRefactorings;

namespace DeclareLocalVariable.DefaultObjectCreateRefactoring {
    [ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = nameof(CreateVarDefaultObjectRefactoringCodeRefactoringProvider)), Shared]
    internal class CreateVarDefaultObjectRefactoringCodeRefactoringProvider : CodeRefactoringProvider {
        private readonly String RefactoringName = "Create var default object";

        public sealed override async Task ComputeRefactoringsAsync(CodeRefactoringContext context) {
            var isCallPoint = await DefaultObjectCodeRefactoring.IsCallPoint(context);
            if (isCallPoint) {
                var action = CodeAction.Create(RefactoringName, c => DefaultObjectCodeRefactoring.CreateDefaultObject(context, true));
                context.RegisterRefactoring(action);
            }
        }
    }
}
