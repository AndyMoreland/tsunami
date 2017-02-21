#!/usr/bin/env node
/// <reference path="../../typings/node/node.d.ts" />

import * as Bluebird from "bluebird";
import * as fs from "fs";
import * as glob from "glob";
import * as ts from "typescript";
import * as yargs from "yargs";
import { ImportBlockBuilder } from "../imports/ImportBlockBuilder";
import { ImportEditor } from "../imports/ImportEditor";
import { SimpleImportBlockFormatter } from "../imports/SimpleImportBlockFormatter";
import { applyCodeEdits } from "../utilities/ioUtils";

const promiseGlob = Bluebird.promisify<string[], string>(glob);
const readFilePromise = Bluebird.promisify(fs.readFile);

function getSourceFileFor(filename: string): Promise<ts.SourceFile> {
    return readFilePromise(filename).then(buffer => {
        return ts.createSourceFile(filename, buffer.toString(), ts.ScriptTarget.ES5, true);
    });
}

const args = yargs.usage("Usage: $0 --indent-size [num] <files>")
    .number("indent-size")
    .default("indent-size", 2)
    .boolean("trailing-comma-in-object-literals")
    .default("trailing-comma-in-object-literals", false)
    .boolean("use-double-quotes")
    .default("use-double-quotes", true)
    .boolean("changes-are-failure")
    .default("changes-are-failure", false)
      .argv;

let changesWereMade = false;

const promises = args._.map(async (input) => {
    const matches = await promiseGlob(input);
    await Promise.all(matches.map(async (filename) => {
        const editor = new ImportEditor(new SimpleImportBlockFormatter({
            indentSize: (args as any).indentSize,
            trailingCommaInObjectLiterals: (args as any).trailingCommaInObjectLiterals,
            useDoubleQuotes: (args as any).useDoubleQuotes,
        }));
        const sourceFile = await getSourceFileFor(filename);
        const importBlock = ImportBlockBuilder.fromFile(sourceFile).build();
        const edits = editor.applyImportBlockToFile(sourceFile, importBlock);
        changesWereMade = changesWereMade || await applyCodeEdits(filename, edits);
        console.log("Edited: ", filename);
    }));
});

Bluebird.all(promises).then(() => {
    if ((args as any).changesAreFailure && changesWereMade) {
        process.exit(1);
    }
});
