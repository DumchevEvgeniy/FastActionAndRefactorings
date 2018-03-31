using System.Linq;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace DeclareLocalVariable {
    public static class SolutionNodeReplacer {
        public static async Task<Solution> Replace(CodeRefactoringContext context, LocalDeclarationStatementSyntax localDeclarationStatement, StatementSyntax oldStatement) {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
            var localStatementWithTrivia = localDeclarationStatement
                .WithLeadingTrivia(oldStatement.GetLeadingTrivia())
                .WithTrailingTrivia(oldStatement.GetTrailingTrivia());
            var block = oldStatement.AncestorsAndSelf().OfType<BlockSyntax>().First();
            var newRoot = root.ReplaceNode(block, block.ReplaceNode(oldStatement, localStatementWithTrivia));
            var newDocument = context.Document.WithSyntaxRoot(newRoot);
            return await LocalVariableRenamer.RenameVariable(newDocument, context);
        }
    }
}
