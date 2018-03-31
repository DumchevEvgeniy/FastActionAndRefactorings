using System.Linq;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Rename;

namespace DeclareLocalVariable {
    public static class LocalVariableRenamer {
        public static async Task<Solution> RenameVariable(Document newDocument, CodeRefactoringContext context) {
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
    }
}
