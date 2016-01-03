import * as ts from "typescript";

enum DefinitionType {
  CLASS_PROPERTY, CLASS_METHOD, FUNCTION, EXPORTED_VAR
}

interface Definition {
  text: string;
  location: number;
  filename: string;
  type: DefinitionType;
};

interface DefinitionIndex {
  [index: string]: Definition;
}

export class Indexer {
  private index: DefinitionIndex;

  indexNode(node: ts.Node): void {
    switch (node.kind) {
      case ts.SyntaxKind.MethodDeclaration:
      break;
      case ts.SyntaxKind.FunctionDeclaration:
      console.log("Hit function declaration node!: ", node);
      break;
      case ts.SyntaxKind.PropertyDeclaration:
      break;
      case ts.SyntaxKind.ExportDeclaration:
      break;
      default: break;
    }
  }

  indexFile(sourceFile: ts.SourceFile): void {
    this.index = {};

    ts.forEachChild(sourceFile, this.indexNode);
  }

  getDefinitionIndex(): DefinitionIndex {
    return this.index;
  }
}
/*
let indexer = new Indexer();
let sourceFile = ts.createSourceFile("/Users/amoreland/tsunami/src/index.ts", readFileSync("/Users/amoreland/tsunami/src/index.ts").toString(), ts.ScriptTarget.ES5, true);
indexer.indexFile(sourceFile);
*/
