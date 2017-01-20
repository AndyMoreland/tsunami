import * as ts from "typescript";
import * as tsu from "@derander/tsunami";

export function addImportToFile(
    sourceFile: ts.SourceFile,
    namespaceAliases: Map<any, string>,
    moduleSpecifier: any,
    symbolName: string,
    options: tsu.InitializedFormatOptions
): tsu.CodeEdit[] {
    const builder = tsu.ImportBlockBuilder.fromFile(sourceFile);
    if (namespaceAliases.has(moduleSpecifier)) {
        builder.addNamespaceSpecifier(
            moduleSpecifier,
            namespaceAliases.get(moduleSpecifier)!
        );
    } else {
        builder.addImportBinding(
            moduleSpecifier,
            { symbolName }
        );
    }

    return (new tsu.ImportEditor(new tsu.SimpleImportBlockFormatter(options)))
        .applyImportBlockToFile(sourceFile, builder.build());
}
