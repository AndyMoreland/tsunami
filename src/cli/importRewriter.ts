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

const args = yargs.usage("Usage: $0 --indent-size [num] --from module#symbol --to module#symbol <files>")
    .string("from")
    .string("to")
    .number("indent-size")
    .default("indent-size", 2)
    .argv;

const [fromModule, fromSymbol] = ((args as any).from).split("#") as string[];
const [toModule, toSymbol] = ((args as any).to).split("#") as string[];

console.log(`Rewriting ${fromModule}.${fromSymbol} -> ${toModule}.${toSymbol}`);
args._.forEach(async (input) => {
    let n = 0;
    const matches = await promiseGlob(input);
    await Bluebird.all(matches.map(async (filename) => {
        let edited = false;
        const editor = new ImportEditor(new SimpleImportBlockFormatter({
            indentSize: (args as any).indentSize
        }));
        const sourceFile = await getSourceFileFor(filename);
        const oldBlock = ImportBlockBuilder.fromFile(sourceFile).build();
        const builder = ImportBlockBuilder.from(oldBlock);
        if (fromSymbol != null) {
            if (oldBlock.mayContainImport(fromModule as any, fromSymbol)) {
                builder.withoutImport(fromModule as any, fromSymbol);
                builder.addImportBinding(toModule as any, { symbolName: toSymbol });
                edited = true;
            }
        } else {
            if (oldBlock.importRecords[fromModule]) {
                builder.renameModule(fromModule as any, toModule as any);
                edited = true;
            }
        }

        if (edited) {
            debugger;
            const edits = editor.applyImportBlockToFile(sourceFile, builder.build());
            await applyCodeEdits(filename, edits);
            n++;
        }
    }));
    console.log(`Edited ${n} files.`);
});
