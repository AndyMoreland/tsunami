#!/usr/bin/env node
/// <reference path="../../typings/node/node.d.ts" />

import * as fs from "fs";
import * as glob from "glob";
import * as ts from "typescript";
import { ImportBlock } from "../imports/ImportBlock";
import { ImportEditor } from "../imports/ImportEditor";
import { SimpleImportBlockFormatter } from "../imports/SimpleImportBlockFormatter";
import { applyCodeEdits } from "../utilities/ioUtils";

const promiseGlob = Promise.promisify<string[], string>(glob);
const readFilePromise = Promise.promisify(fs.readFile);

function getSourceFileFor(filename: string): Promise<ts.SourceFile> {
    return readFilePromise(filename).then(buffer => {
        return ts.createSourceFile(filename, buffer.toString(), ts.ScriptTarget.ES5, true);
    });
}

process.argv.slice(2).forEach(async (input) => {
    const matches = await promiseGlob(input);
    matches.forEach(async (filename) => {
        const editor = new ImportEditor(new SimpleImportBlockFormatter());
        const sourceFile = await getSourceFileFor(filename);
        const importBlock = ImportBlock.fromFile(sourceFile);
        const edits = editor.applyImportBlockToFile(sourceFile, importBlock);
        await applyCodeEdits(filename, edits);
        console.log("Edited: ", filename);
    });
});
