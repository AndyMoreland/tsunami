/// <reference path="../typings/node/node.d.ts" />

import * as fs from "fs";
import * as ts from "typescript";
import { ImportSorter } from './importSorter';

function getSourceFileFor(filename: string): ts.SourceFile {
  return ts.createSourceFile(filename, fs.readFileSync(filename).toString(), ts.ScriptTarget.ES5, true);
}

process.argv.slice(2).forEach((filename) => {
  let sourceFile = getSourceFileFor(filename);
  let sorter = new ImportSorter(sourceFile);
  console.log("Sorting: ", filename);
  try {
    sorter.sortFileImports((err) => {
      if (err) {
        console.log("Failed to sort: ", filename);
      }
    });
  } catch (e) {
    console.log("Failed to sort: ", filename);
  }
});
