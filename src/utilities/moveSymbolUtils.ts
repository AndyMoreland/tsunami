import * as ts from "typescript";
import { ImportBlockBuilder } from "../imports/ImportBlockBuilder";
import { ImportEditor } from "../imports/ImportEditor";
import { ModuleSpecifier } from "../imports/ImportStatement";
import { CodeEditGroup } from "../protocol/types";

export function rewriteSymbolImportInFile(
    sourceFile: ts.SourceFile,
    editor: ImportEditor,
    fromModuleSpecifier: ModuleSpecifier,
    toModuleSpecifier: ModuleSpecifier,
    symbolName: string
): CodeEditGroup | undefined {
    const currentBlock = ImportBlockBuilder.fromFile(sourceFile).build();
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
        return undefined;
    }
}

export function rewriteModuleImportInFile(
    sourceFile: ts.SourceFile,
    editor: ImportEditor,
    fromModuleSpecifier: ModuleSpecifier,
    toModuleSpecifier: ModuleSpecifier
): CodeEditGroup | undefined {
    const currentBlock = ImportBlockBuilder.fromFile(sourceFile).build();
    if (currentBlock.importRecords[fromModuleSpecifier] != null) {
        const newBlock = ImportBlockBuilder.from(currentBlock)
            .renameModule(fromModuleSpecifier, toModuleSpecifier)
            .build();

        const edits = editor.applyImportBlockToFile(sourceFile, newBlock);
        return { file: sourceFile.fileName, edits };
    } else {
        return undefined;
    }
}

export function findNodeForSymbolName(sourceFile: ts.SourceFile, symbolName: string): ts.Node | undefined {
    return (sourceFile.statements || sourceFile.endOfFileToken).find(node => {
        if (node.kind === ts.SyntaxKind.ClassDeclaration) {
            const castNode = node as ts.ClassDeclaration;
            return castNode.name && (castNode.name.getText() === symbolName) || false;
        } else if (node.kind === ts.SyntaxKind.VariableStatement) {
            const castNode = node as ts.VariableStatement;
            const firstDeclaration = castNode.declarationList.declarations.map(declaration => declaration)[0];
            return firstDeclaration.name && (firstDeclaration.name.getText() === symbolName) || false;
        } else if (node.kind === ts.SyntaxKind.FunctionDeclaration) {
            const castNode = node as ts.FunctionDeclaration;
            return castNode.name && (castNode.name.getText() === symbolName) || false;
        } else if (node.kind === ts.SyntaxKind.InterfaceDeclaration) {
            const castNode = node as ts.InterfaceDeclaration;
            return castNode.name && (castNode.name.getText() === symbolName) || false;
        } else if (node.kind === ts.SyntaxKind.EnumDeclaration) {
            const castNode = node as ts.EnumDeclaration;
            return castNode.name && (castNode.name.getText() === symbolName) || false;
        } else if (node.kind === ts.SyntaxKind.TypeAliasDeclaration) {
            const castNode = node as ts.TypeAliasDeclaration;
            return castNode.name && (castNode.name.getText() === symbolName) || false;
        } else if (node.kind === ts.SyntaxKind.ModuleDeclaration) {
            const castNode = node as ts.ModuleDeclaration;
            return castNode.name && ((castNode.name.getText() === symbolName)) || false;
        }

        return false;
    });
}
