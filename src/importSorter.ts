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
    private insideImport: ImportState = ImportState.NEVER_FOUND_IMPORT;
    private importDeclarations: ts.ImportDeclaration[] = [];

    constructor(sourceFile: ts.SourceFile) {
        this.sourceFile = sourceFile;
    }

    private visitNode = (node: ts.Node) => {
        if (node.kind == ts.SyntaxKind.ImportDeclaration) {
            if (this.insideImport == ImportState.LEFT_IMPORT_BLOCK) {
                throw new NonContiguousImportBlockException();
            } else {
                this.insideImport = ImportState.READING_IMPORTS
            }

            this.firstImportPosition = this.firstImportPosition == -1 ? node.getStart() : this.firstImportPosition;
            this.importDeclarations.push(node as ts.ImportDeclaration);
            this.lastImportPosition = node.getEnd();
        } else if (this.insideImport == ImportState.READING_IMPORTS) {
            this.insideImport = ImportState.LEFT_IMPORT_BLOCK;
        }
    }

    static emitImport(importStatement: ts.ImportDeclaration): string {
        let output = importStatement.getText();
        /* let namedBindings = <ts.NamedImports>importStatement.importClause.namedBindings; */
        output = output.replace(/'/g, "\"");
        output = output.replace(/"$/, "\";");
        return output.replace(/\n/g, "");
    }

    static emitModuleSpecifier(moduleSpecifier: ts.Expression): string {
        return moduleSpecifier.getText().replace(/'/g, "\"");
    }

    private createNewImportBlock() {
        let sortedDecs = this.importDeclarations.sort((a, b) => {
            if (ImportSorter.emitModuleSpecifier(a.moduleSpecifier) == ImportSorter.emitModuleSpecifier(b.moduleSpecifier)) {
                return 0;
            }
            return (ImportSorter.emitModuleSpecifier(a.moduleSpecifier) >= ImportSorter.emitModuleSpecifier(b.moduleSpecifier)) ? 1 : -1;
        });

        let prefix = this.firstImportPosition != 0 ? "\n" : "";
        return prefix + sortedDecs.map(ImportSorter.emitImport).join("\n");
    }

    readImportStatements(): void {
        ts.forEachChild(this.sourceFile, this.visitNode);
    }

    sortFileImports(cb: (err?: Error) => void): void {
        if (this.insideImport == ImportState.NEVER_FOUND_IMPORT) {
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
