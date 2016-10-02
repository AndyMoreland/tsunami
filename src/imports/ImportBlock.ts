import * as ts from "typescript";
import {
    createImportStatementFromImportDeclaration,
    ImportRecord
} from "./ImportStatement";

export type ImportRecords = { [canonicalModuleName: string]: ImportRecord };

export class ImportBlock {
    /* Don't use unless you know what you are doing. */
    constructor(public readonly importRecords: ImportRecords = {}) {}

    public static fromFile(sourceFile: ts.SourceFile) {
        const importRecords: ImportRecords = {};

        ts.forEachChild(sourceFile, (node) => {
            if (node.kind === ts.SyntaxKind.ImportDeclaration) {
                const record = createImportStatementFromImportDeclaration(node as ts.ImportDeclaration);
                importRecords[record.moduleSpecifier] = record;
            }
        });

        return new ImportBlock(importRecords);
    }
}
