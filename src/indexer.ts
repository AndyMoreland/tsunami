import log from "./log";
import * as ts from "typescript";

enum DefinitionType {
    FUNCTION, EXPORTED_VAR, INTERFACE, ENUM, CLASS, TYPE, MODULE
}

export interface Definition {
    text: string;
    location: number;
    filename: string;
    type: DefinitionType;
    default: boolean
};

interface FileDefinitionIndex {
    [symbolName: string]: Definition;
}


export class FileIndexer {
    private index: FileDefinitionIndex;

    constructor(private sourceFile: ts.SourceFile) {}

    private addDefinitiontoIndex(definition: Definition): void {
        this.index[definition.text] = definition;
    }

    private getDefinitionForNode(node: ts.Node, name: string, type: DefinitionType, isDefault: boolean = false): Definition {
        return {
            text: name,
            location: node.getStart(),
            filename: this.sourceFile.fileName,
            type: type,
            default: this.isDefaultNode(node)
        };
    }

    private indexClassDeclaration(node: ts.ClassDeclaration): void {
        // log("indexing class: ", node.name.getText());
        this.addDefinitiontoIndex(this.getDefinitionForNode(node, node.name && node.name.getText(), DefinitionType.CLASS));
    }

    private indexVariableStatement(node: ts.VariableStatement): void {
        let firstDeclaration = node.declarationList.declarations.map(declaration => declaration)[0];
        // log("indexing Variable: ", firstDeclaration.name.getText());
        this.addDefinitiontoIndex(this.getDefinitionForNode(firstDeclaration, firstDeclaration && firstDeclaration.name && firstDeclaration.name.getText(), DefinitionType.EXPORTED_VAR));
    }

    private indexFunctionDeclaration(node: ts.FunctionDeclaration): void {
        // log("indexing Function: ", node.name.getText());
        this.addDefinitiontoIndex(this.getDefinitionForNode(node, node.name && node.name.getText(), DefinitionType.FUNCTION));
    }

    private indexInterfaceDeclaration(node: ts.InterfaceDeclaration): void {
        // log("indexing Interface: ", node.name.getText());
        this.addDefinitiontoIndex(this.getDefinitionForNode(node, node.name && node.name.getText(), DefinitionType.INTERFACE));
    }

    private indexEnumDeclaration(node: ts.EnumDeclaration): void {
        // log("indexing Enum: ", node.name.getText());
        this.addDefinitiontoIndex(this.getDefinitionForNode(node, node.name && node.name.getText(), DefinitionType.ENUM));
    }

    private indexTypeAliasDeclaration(node: ts.TypeAliasDeclaration): void {
        // log("indexing Type Alias: ", node.name.getText());
        this.addDefinitiontoIndex(this.getDefinitionForNode(node, node.name && node.name.getText(), DefinitionType.TYPE));
    }

    private indexModuleDeclaration(node: ts.ModuleDeclaration): void {
        // log("indexing Module: ", node.name.getText());
        this.addDefinitiontoIndex(this.getDefinitionForNode(node, node.name && node.name.getText(), DefinitionType.MODULE));
    }

    private indexExportDeclaration(node: ts.Node): void {
        // log("Found export declaration.");
        if (node.kind == ts.SyntaxKind.ClassDeclaration) {
            this.indexClassDeclaration(node as ts.ClassDeclaration);
        } else if (node.kind == ts.SyntaxKind.VariableStatement) {
            this.indexVariableStatement(node as ts.VariableStatement);
        } else if (node.kind == ts.SyntaxKind.FunctionDeclaration) {
            this.indexFunctionDeclaration(node as ts.FunctionDeclaration);
        } else if (node.kind == ts.SyntaxKind.InterfaceDeclaration) {
            this.indexInterfaceDeclaration(node as ts.InterfaceDeclaration);
        } else if (node.kind == ts.SyntaxKind.EnumDeclaration) {
            this.indexEnumDeclaration(node as ts.EnumDeclaration);
        } else if (node.kind == ts.SyntaxKind.TypeAliasDeclaration) {
            this.indexTypeAliasDeclaration(node as ts.TypeAliasDeclaration);
        } else if (node.kind == ts.SyntaxKind.ModuleDeclaration) {
            this.indexModuleDeclaration(node as ts.ModuleDeclaration);
        } else {
            log("Failed to understand node kind: ", node.kind);
        }
    }

    private isDefaultNode(node: ts.Node): boolean {
        return node.modifiers && node.modifiers.filter(modifierNode => modifierNode.kind == ts.SyntaxKind.DefaultKeyword).length > 0
    }

    private isExportedNode(node: ts.Node): boolean {
        return node.modifiers && node.modifiers.filter(modifierNode => modifierNode.kind == ts.SyntaxKind.ExportKeyword).length > 0
    }

    indexNode = (node: ts.Node) => {
        if (this.isExportedNode(node)) {
            this.indexExportDeclaration(node);
        }
    }

    indexFile(): void {
        this.index = {};

        try {
            ts.forEachChild(this.sourceFile, this.indexNode);
        } catch (e) {
            log("Failed during indexing of file ", this.sourceFile.fileName, ": ", e);
        }
    }

    getDefinitionIndex(): FileDefinitionIndex {
        return this.index;
    }
}
