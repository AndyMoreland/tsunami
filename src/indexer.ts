import log from "./log";
import * as ts from "typescript";

export const FISH = "hi";

enum DefinitionType {
  CLASS_PROPERTY, CLASS_METHOD, FUNCTION, EXPORTED_VAR, INTERFACE, ENUM, CLASS
}

export interface Definition {
  text: string;
  location: number;
  filename: string;
  type: DefinitionType;
};

interface FileDefinitionIndex {
  [index: string]: Definition;
}

export class FileIndexer {
  private index: FileDefinitionIndex;

  constructor(private sourceFile: ts.SourceFile) {}

  private indexClassDeclaration(node: ts.ClassDeclaration): void {
    log("indexing class: ", node.name.getText());
  }

  private indexVariableDeclaration(node: ts.VariableDeclaration): void {
    log("indexing variable: ", node.name.getText());
  }

  private indexFunctionDeclaration(node: ts.FunctionDeclaration): void {
    log("indexing Function: ", node.name.getText());
  }

  private indexInterfaceDeclaration(node: ts.InterfaceDeclaration): void {
    log("indexing Interface: ", node.name.getText());
  }

  private indexEnumDeclaration(node: ts.EnumDeclaration): void {
    log("indexing Enum: ", node.name.getText());
  }

  private indexExportDeclaration(node: ts.Node): void {
    log("Found export declaration.");
    if (node.kind == ts.SyntaxKind.ClassDeclaration) {
      this.indexClassDeclaration(node as ts.ClassDeclaration);
    } else if (node.kind == ts.SyntaxKind.VariableDeclaration) {
      this.indexVariableDeclaration(node as ts.VariableDeclaration);
    } else if (node.kind == ts.SyntaxKind.FunctionDeclaration) {
      this.indexFunctionDeclaration(node as ts.FunctionDeclaration);
    } else if (node.kind == ts.SyntaxKind.InterfaceDeclaration) {
      this.indexInterfaceDeclaration(node as ts.InterfaceDeclaration);
    } else if (node.kind == ts.SyntaxKind.EnumDeclaration) {
      this.indexEnumDeclaration(node as ts.EnumDeclaration);
    }
  }

  indexNode = (node: ts.Node) => {
    if (node.modifiers && node.modifiers.filter(modifierNode => modifierNode.kind == ts.SyntaxKind.ExportKeyword).length > 0) {
      this.indexExportDeclaration(node);
    }
  }

  indexFile(): void {
    this.index = {};

    ts.forEachChild(this.sourceFile, this.indexNode);
  }

  getDefinitionIndex(): FileDefinitionIndex {
    return this.index;
  }
}
