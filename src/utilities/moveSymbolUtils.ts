import * as ts from "typescript";
import { ImportBlock } from "../imports/ImportBlock";
import { ImportBlockBuilder } from "../imports/ImportBlockBuilder";
import { ImportEditor } from "../imports/ImportEditor";
import { ModuleSpecifier } from "../imports/ImportStatement";
import { CodeEditGroup } from "../protocol/types";

export function rewriteFile(
    sourceFile: ts.SourceFile,
    editor: ImportEditor,
    fromModuleSpecifier: ModuleSpecifier,
    toModuleSpecifier: ModuleSpecifier,
    symbolName: string
): CodeEditGroup | any[] {
    const currentBlock = ImportBlock.fromFile(sourceFile);
    if (currentBlock.mayContainImport(fromModuleSpecifier, symbolName)) {
        const currentName = currentBlock.getCurrentName(fromModuleSpecifier, symbolName);
        const newBlock = ImportBlockBuilder.from(currentBlock)
            .withoutImport(fromModuleSpecifier, symbolName)
            .addImportBinding(toModuleSpecifier, {
                symbolName,
                alias: currentName !== symbolName ? currentName : undefined
            })
            .build();

        const edits = editor.applyImportBlockToFile(sourceFile, newBlock);
        return { file: sourceFile.fileName, edits };
    } else {
        return [];
    }
}
