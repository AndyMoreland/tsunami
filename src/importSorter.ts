import * as Promise from "bluebird";
import * as path from "path";
import * as ts from "typescript";
import { ImportBlock } from "./imports/ImportBlock";
import { SimpleImportBlockFormatter } from "./imports/SimpleImportBlockFormatter";
import log from "./log";
import { CodeEdit } from "./protocol/types";
import { convertPositionToLocation } from "./utilities/languageUtilities";

export class NonContiguousImportBlockException extends Error {
    constructor(error?: string) {
        super(error);
    }
}

enum ImportState {
    NEVER_FOUND_IMPORT, READING_IMPORTS, LEFT_IMPORT_BLOCK
}

/**
 * Sorts imports lexographically according to (`distance` from the current module, alphabetical ordering).
 * `Distance` is the number of "../" required to reach the LCA of the current module and the imported module.
 * Example sorted import block:
 * import { foo } from "lodash"; //  10000
 * import { bar } from "@AndyMoreland/tsunami"; // 50000
 * import { baz } from "../../../../many/folders/away"; // 4
 * import { banana } from "../../../../z/many/folders/away"; // 4, alphabetically after the previous
 * import { somethingClose } from "./Neighbor"; // 0, in the same folder.
 */
export class ImportSorter {
    private sourceFile: ts.SourceFile;
    private firstImportPosition: number = -1;
    private lastImportPosition: number = -1;
    private importReadingState: ImportState = ImportState.NEVER_FOUND_IMPORT;
    private importDeclarations: ts.ImportDeclaration[] = [];
    constructor(sourceFile: ts.SourceFile) {
        this.sourceFile = sourceFile;
    }

    private visitNode = (node: ts.Node) => {
        if (node.kind === ts.SyntaxKind.ImportDeclaration) {
            if (this.importReadingState === ImportState.LEFT_IMPORT_BLOCK) {
                throw new NonContiguousImportBlockException();
            } else {
                this.importReadingState = ImportState.READING_IMPORTS;
            }

            this.firstImportPosition = this.firstImportPosition === -1 ? node.getStart() : this.firstImportPosition;
            this.importDeclarations.push(node as ts.ImportDeclaration);
            this.lastImportPosition = node.getEnd();
        } else if (this.importReadingState === ImportState.READING_IMPORTS) {
            this.importReadingState = ImportState.LEFT_IMPORT_BLOCK;
        }
    }

    public readImportStatements(): void {
        ts.forEachChild(this.sourceFile, this.visitNode);
    }

    public getSortFileImports(): Promise<CodeEdit | null> {
        try {
            if (this.importReadingState === ImportState.NEVER_FOUND_IMPORT) {
                this.readImportStatements();
            }

            if (this.importDeclarations.length > 0) {
                const formatter = new SimpleImportBlockFormatter();
                const importBlock = ImportBlock.fromFile(this.sourceFile);
                const editFromNewText: CodeEdit = {
                    start: convertPositionToLocation(this.sourceFile, this.firstImportPosition),
                    end: convertPositionToLocation(this.sourceFile, this.lastImportPosition),
                    newText: formatter.formatImportBlock(path.dirname(this.sourceFile.fileName), importBlock)
                };

                return Promise.resolve(editFromNewText);
            }

            return Promise.resolve(null);
        } catch (e) {
            return Promise.reject(e);
        }
    }
}
