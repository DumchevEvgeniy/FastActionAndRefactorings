using System;
using Microsoft.CodeAnalysis;
using System.Linq;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Generic;

namespace DeclareLocalVariable.DeclareClassRefactoring {
    public static class CodeElementFactory {
        public static PropertyDeclarationSyntax CreateProperty(TypeSyntax typeName, String propertyName) {
            var accessorList = SyntaxFactory.AccessorList().AddAccessors(
                SyntaxFactory.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration).WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.SemicolonToken)),
                SyntaxFactory.AccessorDeclaration(SyntaxKind.SetAccessorDeclaration).WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.SemicolonToken))
            );
            return SyntaxFactory.PropertyDeclaration(new SyntaxList<AttributeListSyntax>(),
                SyntaxTokenList.Create(SyntaxFactory.Token(SyntaxKind.PublicKeyword)),
                typeName,
                null,
                SyntaxFactory.Identifier(propertyName),
                accessorList
            );
        } 

        public static ConstructorDeclarationSyntax CreateConstructor(String className, IEnumerable<TypeSyntax> parameterTypes) {
            return SyntaxFactory.ConstructorDeclaration(new SyntaxList<AttributeListSyntax>(),
                SyntaxTokenList.Create(SyntaxFactory.Token(SyntaxKind.PublicKeyword)),
                SyntaxFactory.Identifier(className),
                CreateParameterList(parameterTypes),
                null,
                SyntaxFactory.Block()
            );
        }

        public static ParameterListSyntax CreateParameterList(IEnumerable<TypeSyntax> parameterTypes) {
            var parameterList = SyntaxFactory.ParameterList();
            if (parameterTypes == null || !parameterTypes.Any())
                return parameterList;
            Int32 index = 1;
            foreach (var parameterType in parameterTypes) {
                var parameterName = "arg" + index.ToString();
                var parameterIdentifierName = SyntaxFactory.Identifier(parameterName);
                var parameter = SyntaxFactory.Parameter(new SyntaxList<AttributeListSyntax>(), new SyntaxTokenList(), parameterType, parameterIdentifierName, null);
                parameterList = parameterList.AddParameters(parameter);
                index++;
            }
            return parameterList;
        }

        public static MethodDeclarationSyntax CreareMethodDeclaration(TypeSyntax returnType, String methodName, IEnumerable<TypeSyntax> parameterTypes) {
            return SyntaxFactory.MethodDeclaration(new SyntaxList<AttributeListSyntax>(),
                SyntaxTokenList.Create(SyntaxFactory.Token(SyntaxKind.PublicKeyword)),
                returnType,
                null,
                SyntaxFactory.Identifier(methodName),
                null,
                CreateParameterList(parameterTypes),
                new SyntaxList<TypeParameterConstraintClauseSyntax>(),
                SyntaxFactory.Block(),
                null
            );
        }

        public static TypeSyntax CreateType(SyntaxKind syntaxKind) => SyntaxFactory.IdentifierName(SyntaxFactory.Token(syntaxKind).Text);
        public static TypeSyntax CreateType(String name) => SyntaxFactory.IdentifierName(name);
        public static TypeSyntax CreateType(TypeInfo typeInfo) {
            var type = typeInfo.Type;
            if (type == null || !type.IsType || type is IErrorTypeSymbol)
                return CreateType(SyntaxKind.ObjectKeyword);
            var namedType = type as INamedTypeSymbol;
            if (namedType == null)
                return CreateType(SyntaxKind.ObjectKeyword);
            if (namedType.SpecialType == SpecialType.System_Void)
                return CreateType(SyntaxKind.VoidKeyword);
            if (type.Name == "var")
                return CreateType(SyntaxKind.ObjectKeyword);
            if (namedType.TypeArguments.ToList().Exists(t => t == null || t is IErrorTypeSymbol || t.SpecialType == SpecialType.System_Void))
                return CreateType(SyntaxKind.ObjectKeyword);
            return CreateType(namedType.MetadataName);
        }
    }
}
