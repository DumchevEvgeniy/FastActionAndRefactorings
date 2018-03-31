using System;
using System.Collections.Generic;
using System.Linq;

namespace DeclareLocalVariable.DeclareLocalVariableRefactoring {
    internal static class LocalNameByMathodNameGenerator {
        private static readonly IEnumerable<String> startsDefaultNames = new List<String> {
            "to", "try", "set", "create", "get", "with", "without"
        };

        public static String CreateIdentifierName(String methodName) {
            var newStr = GetName(methodName);
            return Char.ToLower(newStr.First()) + newStr.Remove(0, 1);
        }

        private static String GetName(String methodName) {
            foreach (var startsDefaultName in startsDefaultNames)
                if (StartsWith(methodName, startsDefaultName))
                    return methodName.Remove(0, startsDefaultName.Length);
            return methodName;
        }
        private static Boolean StartsWith(String methodName, String startsDefaultName) {
            if (methodName.Length <= startsDefaultName.Length)
                return false;
            for (Int32 i = 0; i < startsDefaultName.Length; i++)
                if (Char.ToLower(methodName[i]) != startsDefaultName[i])
                    return false;
            return true;
        }
    }
}
