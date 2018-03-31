using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp;

namespace DeclareLocalVariable {
    internal static class LocalVariableNameGenerator {
        public static String Create(String name, SyntaxNode node, SemanticModel semanticModel) {
            var localVariables = GetNamesAllLocalVariables(node, semanticModel);
            return GetLocalVariableName(name, localVariables);
        }

        private static List<String> GetNamesAllLocalVariables(SyntaxNode node, SemanticModel semanticModel) {
            var resultList = new List<String>();
            var declaration = node.Ancestors().OfType<BaseMethodDeclarationSyntax>().First();
            var declarationInfo = semanticModel.GetDeclaredSymbol(declaration);
            var parameterNames = declarationInfo.Parameters.Select(p => p.Name);
            if (parameterNames != null)
                resultList.AddRange(parameterNames);
            foreach (var local in declaration.DescendantNodes().OfType<LocalDeclarationStatementSyntax>())
                foreach (var variable in local.Declaration.Variables)
                    resultList.Add(semanticModel.GetDeclaredSymbol(variable).Name);
            return resultList;
        }

        private static String GetLocalVariableName(String name, List<String> localVariables) {
            if (localVariables == null || localVariables.Count == 0)
                return name;
            var result = name;
            var number = 0;
            while (localVariables.Contains(result))
                result = name + (++number).ToString();
            return result;
        }
    }
}
