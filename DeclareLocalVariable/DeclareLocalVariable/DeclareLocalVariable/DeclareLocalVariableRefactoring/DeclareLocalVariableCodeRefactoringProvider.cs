using System;
using System.Composition;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeRefactorings;

namespace DeclareLocalVariable.DeclareLocalVariableRefactoring {
    [ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = nameof(DeclareLocalVariableCodeRefactoringProvider)), Shared]
    public class DeclareLocalVariableCodeRefactoringProvider : CodeRefactoringProvider {
        private readonly String refactoringName = "Declare var local variable";

        public sealed override async Task ComputeRefactoringsAsync(CodeRefactoringContext context) {
            var isCallPoint = await LocalVariableCodeRefactoring.IsCallPoint(context);
            if (isCallPoint) {
                var action = CodeAction.Create(refactoringName, c => LocalVariableCodeRefactoring.DeclareLocalVariable(context, "var"));
                context.RegisterRefactoring(action);
            }
        }
    }
}
