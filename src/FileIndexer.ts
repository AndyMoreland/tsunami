import * as Bluebird from "bluebird";
import * as fs from "fs";
import * as path from "path";
import * as ts from "typescript";
import { Definition, DefinitionIndex, DefinitionType } from "./Indexer";
import { ModuleSpecifier } from "./imports/ImportStatement";
import log from "./log";
import { isExportedNode } from "./utilities/languageUtilities";

export class FileIndexer {
    private index: DefinitionIndex;

    constructor(
        private moduleSpecifier: ModuleSpecifier,
        private sourceFile: ts.SourceFile,
        private getSourceFileForAbsolutePath?: (
            absolutePath: string
        ) => Promise<ts.SourceFile>
    ) {}

    private addDefinitiontoIndex(definition: Definition): void {
        if (definition.text != null) {
            this.index.set(definition.text, definition);
        }
    }

    private addIndex(index: DefinitionIndex): void {
        index.forEach(def => {
            if (def.text != null) {
                this.index.set(def.text!, def);
            }
        });
    }

    private getDefinitionForNode(
        node: ts.Node,
        name: string,
        type: DefinitionType,
        isDefault: boolean = false
    ): Definition {
        return {
            text: name,
            span: {
                start: node.getStart(),
                end: node.getEnd()
            },
            moduleSpecifier: this.moduleSpecifier,
            type: type,
            default: this.isDefaultNode(node)
        };
    }

    private indexClassDeclaration(node: ts.ClassDeclaration): void {
        try {
            this.addDefinitiontoIndex(
                this.getDefinitionForNode(
                    node,
                    (node.name != null && node.name.getText()) || "",
                    DefinitionType.CLASS
                )
            );
        } catch (e) {
            log("Node of death encountered.");
            log(e.stack);
            log(JSON.stringify(node, null, 2));
        }
    }

    private indexVariableStatement(node: ts.VariableStatement): void {
        let firstDeclaration = node.declarationList.declarations.map(
            declaration => declaration
        )[0];
        this.addDefinitiontoIndex(
            this.getDefinitionForNode(
                firstDeclaration,
                firstDeclaration &&
                    firstDeclaration.name &&
                    firstDeclaration.name.getText(),
                DefinitionType.EXPORTED_VAR
            )
        );
    }

    private indexFunctionDeclaration(node: ts.FunctionDeclaration): void {
        this.addDefinitiontoIndex(
            this.getDefinitionForNode(
                node,
                node.name != null ? node.name.getText() : "",
                DefinitionType.FUNCTION
            )
        );
    }

    private indexInterfaceDeclaration(node: ts.InterfaceDeclaration): void {
        this.addDefinitiontoIndex(
            this.getDefinitionForNode(
                node,
                node.name && node.name.getText(),
                DefinitionType.INTERFACE
            )
        );
    }

    private indexEnumDeclaration(node: ts.EnumDeclaration): void {
        this.addDefinitiontoIndex(
            this.getDefinitionForNode(
                node,
                node.name && node.name.getText(),
                DefinitionType.ENUM
            )
        );
    }

    private indexTypeAliasDeclaration(node: ts.TypeAliasDeclaration): void {
        this.addDefinitiontoIndex(
            this.getDefinitionForNode(
                node,
                node.name && node.name.getText(),
                DefinitionType.TYPE
            )
        );
    }

    private indexModuleDeclaration(node: ts.ModuleDeclaration): void {
        this.addDefinitiontoIndex(
            this.getDefinitionForNode(
                node,
                node.name && node.name.getText(),
                DefinitionType.MODULE
            )
        );
    }

    private indexExportedSymbol(node: ts.Node): void {
        if (node.kind === ts.SyntaxKind.ClassDeclaration) {
            this.indexClassDeclaration(node as ts.ClassDeclaration);
        } else if (node.kind === ts.SyntaxKind.VariableStatement) {
            this.indexVariableStatement(node as ts.VariableStatement);
        } else if (node.kind === ts.SyntaxKind.FunctionDeclaration) {
            this.indexFunctionDeclaration(node as ts.FunctionDeclaration);
        } else if (node.kind === ts.SyntaxKind.InterfaceDeclaration) {
            this.indexInterfaceDeclaration(node as ts.InterfaceDeclaration);
        } else if (node.kind === ts.SyntaxKind.EnumDeclaration) {
            this.indexEnumDeclaration(node as ts.EnumDeclaration);
        } else if (node.kind === ts.SyntaxKind.TypeAliasDeclaration) {
            this.indexTypeAliasDeclaration(node as ts.TypeAliasDeclaration);
        } else if (node.kind === ts.SyntaxKind.ModuleDeclaration) {
            this.indexModuleDeclaration(node as ts.ModuleDeclaration);
        } else {
            log("Failed to understand node kind: ", node.kind);
        }
    }

    private indexExportDeclaration(node: ts.ExportDeclaration): Promise<void> {
        const promises: Promise<void>[] = [];

        if (node.exportClause && node.exportClause) {
            node.exportClause
                .getChildren()
                .forEach(specifier =>
                    this.indexExportSpecifier(specifier as ts.ExportSpecifier)
                );
        }

        node.getChildren().forEach(async child => {
            if (child.kind === ts.SyntaxKind.AsteriskToken) {
                const dirname = path.dirname(this.sourceFile.fileName);

                if (
                    node.moduleSpecifier != null &&
                    this.getSourceFileForAbsolutePath !== undefined
                ) {
                    const specifier = node.moduleSpecifier
                        .getText()
                        .replace(/"|'/g, "");
                    const recursivePaths = [
                        path.join(dirname, specifier) + ".d.ts",
                        path.join(dirname, specifier, "index") + ".d.ts",
                        path.join(dirname, specifier) + ".ts",
                        path.join(dirname, specifier) + ".tsx",
                        path.join(dirname, specifier, "index") + ".ts",
                        path.join(dirname, specifier, "index") + ".tsx"
                    ];
                    for (let recursivePath of recursivePaths) {
                        promises.push(this.indexRecursivePath(recursivePath));
                    }
                }
            }
        });

        return Promise.resolve(Bluebird.all(promises).thenReturn());
    }

    private async indexRecursivePath(recursivePath: string): Promise<void> {
        if (
            !fs.existsSync(recursivePath) ||
            !this.getSourceFileForAbsolutePath
        ) {
            return;
        }

        log(
            `[${this.moduleSpecifier}] ${
                this.sourceFile.fileName
            } -> ${recursivePath}`
        );

        try {
            const sourceFile = await this.getSourceFileForAbsolutePath(
                recursivePath
            );
            const indexer = new FileIndexer(
                this.moduleSpecifier,
                sourceFile,
                this.getSourceFileForAbsolutePath
            );
            await indexer.indexFile();
            const recursiveIndex = indexer.getDefinitionIndex();
            log(
                "indexed recursive path",
                recursivePath,
                JSON.stringify(indexer.getDefinitionIndex)
            );
            this.addIndex(recursiveIndex);
        } catch (e) {
            log(
                "Failed to index recursive module: ",
                recursivePath,
                e,
                e.stack
            );
        }
    }

    private indexExportSpecifier(node: ts.ExportSpecifier): void {
        this.addDefinitiontoIndex(
            this.getDefinitionForNode(
                node,
                node.name.getText(),
                DefinitionType.EXPORTED_VAR
            )
        );
    }

    private isDefaultNode(node: ts.Node): boolean {
        return (
            node.modifiers != null &&
            node.modifiers.filter(
                modifierNode =>
                    modifierNode.kind === ts.SyntaxKind.DefaultKeyword
            ).length > 0
        );
    }

    private indexNode = (node: ts.Node): Promise<void> => {
        if (isExportedNode(node)) {
            return Promise.resolve(this.indexExportedSymbol(node));
        } else if (node.kind === ts.SyntaxKind.ExportDeclaration) {
            return this.indexExportDeclaration(node as ts.ExportDeclaration);
        }

        return Promise.resolve<void>(null!);
    };

    public indexFile(): Promise<void> {
        this.index = new Map<string, Definition>();
        const promises: Promise<void>[] = [];

        try {
            ts.forEachChild(this.sourceFile, node => {
                promises.push(this.indexNode(node));
            });
        } catch (e) {
            log(
                "Failed during indexing of file ",
                this.sourceFile.fileName,
                ": ",
                e.stack
            );
        }

        return Promise.resolve(Bluebird.all(promises).thenReturn());
    }

    public getDefinitionIndex(): DefinitionIndex {
        return this.index;
    }
}
