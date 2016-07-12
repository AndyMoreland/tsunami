import * as fs from "fs";
import * as ts from "typescript";

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

    private static emitImport(importStatement: ts.ImportDeclaration): string {
        let output = importStatement.getText();
        /* let namedBindings = <ts.NamedImports>importStatement.importClause.namedBindings; */
        output = output.replace(/'/g, "\"");
        output = output.replace(/"$/, "\";");
        return output.replace(/\n/g, "");
    }

    private static emitModuleSpecifier(moduleSpecifier: ts.Expression): string {
        return moduleSpecifier.getText().replace(/'/g, "\"");
    }

    /* Distance is in [0, inf]. Smaller distance => closer in the file tree. */
    private static computeDistance(specifier: string) {
        if (specifier.slice(1, 3) === "./") {
            return 0;
        }

        const firstChar = specifier.charAt(1);

        /* For imports from dependencies, we want 3rd party dependencies listed before
           internal dependencies. */

        if (firstChar !== ".") {
            if (firstChar === "@") {
                return 5000;
            }

            if (specifier.slice(1, 4) !== "../") {
                return 10000;
            }
        }

        return (specifier.match(/..\//g) || []).length;
    }

    private createNewImportBlock() {
        /* Sort from farthest-away-to-closest-away, top-to-bottom */
        let sortedDecs = this.importDeclarations.sort((a, b) => {
            let aSpecifier = ImportSorter.emitModuleSpecifier(a.moduleSpecifier);
            let bSpecifier = ImportSorter.emitModuleSpecifier(b.moduleSpecifier);

            let aDistance = ImportSorter.computeDistance(aSpecifier);
            let bDistance = ImportSorter.computeDistance(bSpecifier);

            if (aDistance !== bDistance) {
                return aDistance > bDistance ? -1 : 1;
            }

            if (aSpecifier === bSpecifier) {
                return 0;
            }

            return aSpecifier >= bSpecifier ? 1 : -1;
        });

        let prefix = this.firstImportPosition !== 0 ? "\n" : "";
        return prefix + sortedDecs.map(ImportSorter.emitImport).join("\n");
    }

    public readImportStatements(): void {
        ts.forEachChild(this.sourceFile, this.visitNode);
    }

    public sortFileImports(cb: (err?: Error) => void): void {
        if (this.importReadingState === ImportState.NEVER_FOUND_IMPORT) {
            this.readImportStatements();
        }

        if (this.importDeclarations.length > 0) {
            let text = this.sourceFile.getFullText();
            let header = text.substring(0, this.firstImportPosition - 1);
            let importBlock = this.createNewImportBlock();
            let footer = text.substring(this.lastImportPosition);

            let newText = header + importBlock + footer;
            fs.writeFile(this.sourceFile.fileName, newText, cb);
        }
    }
}
