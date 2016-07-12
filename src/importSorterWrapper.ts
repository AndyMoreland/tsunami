/// <reference path="../typings/node/node.d.ts" />

import * as fs from "fs";
import * as ts from "typescript";
import { ImportSorter } from "./importSorter";

function getSourceFileFor(filename: string): ts.SourceFile {
    return ts.createSourceFile(filename, fs.readFileSync(filename).toString(), ts.ScriptTarget.ES5, true);
}

process.argv.slice(2).forEach((filename) => {
    let sourceFile = getSourceFileFor(filename);
    let sorter = new ImportSorter(sourceFile);
    console.log("Sorting: ", filename);
    sorter.sortFileImports().then(() => {
        console.log("Sorted!");
    }).catch(e => console.error("Failed to sort", filename, e));
});
