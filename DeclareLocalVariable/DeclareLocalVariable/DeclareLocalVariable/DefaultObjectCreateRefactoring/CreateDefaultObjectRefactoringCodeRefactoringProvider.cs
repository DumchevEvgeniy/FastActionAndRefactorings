using System;
using System.Composition;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeRefactorings;

namespace DeclareLocalVariable.DefaultObjectCreateRefactoring {
    [ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = nameof(CreateDefaultObjectRefactoringCodeRefactoringProvider)), Shared]
    internal class CreateDefaultObjectRefactoringCodeRefactoringProvider : CodeRefactoringProvider {
        private readonly String refactoringName = "Create default object";

        public sealed override async Task ComputeRefactoringsAsync(CodeRefactoringContext context) {
            var isCallPoint = await DefaultObjectCodeRefactoring.IsCallPoint(context);
            if (isCallPoint) {
                var action = CodeAction.Create(refactoringName, c => DefaultObjectCodeRefactoring.CreateDefaultObject(context, false));
                context.RegisterRefactoring(action);
            }
        }
    }
}