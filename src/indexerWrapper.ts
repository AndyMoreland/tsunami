/// <reference path="../typings/node/node.d.ts" />

import { FileIndexer } from "./FileIndexer";
import * as fs from "fs";
import * as ts from "typescript";

function getSourceFileFor(filename: string): ts.SourceFile {
  return ts.createSourceFile(filename, fs.readFileSync(filename).toString(), ts.ScriptTarget.ES5, true);
}

process.argv.slice(2).forEach((filename) => {
  let sourceFile = getSourceFileFor(filename);
  let fileIndexer = new FileIndexer(sourceFile);
  console.log("Indexing: ", filename);
  try {
      fileIndexer.indexFile();
      let index = fileIndexer.getDefinitionIndex();
      console.log(index);
  } catch (e) {
    console.error("Failed to index: ", filename);
  }
});
