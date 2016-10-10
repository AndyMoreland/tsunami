/// <reference path="../../typings/node/node.d.ts" />

import * as fs from "fs";
import * as ts from "typescript";
import { AbsoluteFilename } from "../imports/ImportStatement";
import { FileIndexer } from "../FileIndexer";

function getSourceFileFor(filename: string): ts.SourceFile {
    return ts.createSourceFile(filename, fs.readFileSync(filename).toString(), ts.ScriptTarget.ES5, true);
}

async function processFilename(filename: string) {
    const sourceFile = getSourceFileFor(filename);
    const fileIndexer = new FileIndexer(
        sourceFile.fileName as AbsoluteFilename,
        sourceFile,
        (filename: string) => Promise.resolve(getSourceFileFor(filename))
    );

    console.log("Indexing: ", filename);
    try {
        await fileIndexer.indexFile();
        let index = fileIndexer.getDefinitionIndex();
        console.log(index);
    } catch (e) {
        console.error("Failed to index: ", filename);
    }
}

process.argv.slice(2).forEach(processFilename);
