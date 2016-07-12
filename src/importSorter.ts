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

    private static computeProximity(specifier: string) {
        if (specifier.slice(0, 3) === "\"./") {
            return 0;
        }

        if (specifier.slice(0, 4) !== "\"../") {
            return 10000;
        }

        return (specifier.match(/..\//g) || []).length;
    }

    private createNewImportBlock() {
        let sortedDecs = this.importDeclarations.sort((a, b) => {
            let aSpecifier = ImportSorter.emitModuleSpecifier(a.moduleSpecifier);
            let bSpecifier = ImportSorter.emitModuleSpecifier(b.moduleSpecifier);

            let aProximity = ImportSorter.computeProximity(aSpecifier);
            let bProximity = ImportSorter.computeProximity(bSpecifier);

            if (aProximity !== bProximity) {
                return aProximity > bProximity ? -1 : 1;
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
