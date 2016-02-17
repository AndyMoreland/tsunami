import * as ts from "typescript";
import * as path from "path";
import { TsProject } from "./tsProject";

class ImportIndexerVisitor {
    private moduleSpecifiers: string[] = [];

    constructor(private indexer: ImportIndexer) {}

    public visitNode = (node: ts.Node) => {
        if (node.kind == ts.SyntaxKind.ImportDeclaration) {
            let importNode = node as ts.ImportDeclaration;
            let moduleSpecifier = importNode.moduleSpecifier.getText()
                .replace(/"/g, "")
                .replace(/'/g, "");

            this.moduleSpecifiers.push(this.indexer.resolveModuleName(moduleSpecifier));
        }
    }

    public getModuleSpecifiers() {
        return this.moduleSpecifiers;
    }
}

export class ImportIndexer {
    constructor(private sourceFile: ts.SourceFile) {}

    public static filenameToModuleName(filename: string): string {
        return filename
            .replace(".ts", "")
            .replace(".tsx", "");
    }

    public absoluteFilenameToRelativeFilename(absoluteFilename: string): string {
        return path.relative(path.dirname(this.sourceFile.fileName), absoluteFilename);
    }

    public resolveModuleName(moduleName: string): string {
        /* Node module */
        if (moduleName[0] != '.' && moduleName[0] != '/') {
            return moduleName;
        } else {
            return path.resolve(path.dirname(this.sourceFile.fileName), moduleName);
        }
    }

    public getImportedModules(): string[] {
        let visitor = new ImportIndexerVisitor(this);
        ts.forEachChild(this.sourceFile, visitor.visitNode);

        return visitor.getModuleSpecifiers();
    }

    public importsModule(absoluteModuleName: string): boolean {
        let visitor = new ImportIndexerVisitor(this);
        ts.forEachChild(this.sourceFile, visitor.visitNode);
        let moduleSpecifiers = visitor.getModuleSpecifiers();
        let needle = ImportIndexer.filenameToModuleName(absoluteModuleName);

        return moduleSpecifiers.indexOf(needle) >= 0;
    }
}

export class ReferenceFinder {
    constructor(private tsProject: TsProject, private getSourceFileFor: (filename: string) => ts.SourceFile) {}

    /* Absolute path to file or node module name */
    findModulesImportingModule(moduleName: string): Promise<string[]> {
        return this.tsProject.getFileNames()
            .then(files => {
                let filteredFiles = files.filter(file => {
                    let indexer = new ImportIndexer(this.getSourceFileFor(file));

                    return indexer.importsModule(moduleName);
                });

                return filteredFiles;
            });
    }
}
