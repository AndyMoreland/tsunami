/// <reference path="../../typings/node/node.d.ts" />

import { ModuleSpecifier } from "../imports/ImportStatement";
import { ImportBlockBuilder } from "../imports/ImportBlockBuilder";
import * as fs from "fs";
import * as ts from "typescript";
import * as Promise from "bluebird";
import { ImportBlock } from "../imports/ImportBlock";
import { ImportEditor } from "../imports/ImportEditor";
import { SimpleImportBlockFormatter } from "../imports/SimpleImportBlockFormatter";
import { applyCodeEdits } from "../utilities/ioUtils";

const readFilePromise = Promise.promisify(fs.readFile);

function getSourceFileFor(filename: string): Promise<ts.SourceFile> {
    return readFilePromise(filename).then(buffer => {
        return ts.createSourceFile(filename, buffer.toString(), ts.ScriptTarget.ES5, true);
    });
}

process.argv.slice(2).forEach((filename) => {
    const editor = new ImportEditor(new SimpleImportBlockFormatter());
    getSourceFileFor(filename).then(sourceFile => {
        const importBlock = ImportBlockBuilder.from(ImportBlock.fromFile(sourceFile))
            .addNamespaceSpecifier("react" as ModuleSpecifier, "React").build();
        const edits = editor.applyImportBlockToFile(sourceFile, importBlock);

        applyCodeEdits(filename, edits)
            .then(() => console.log("Edited: ", filename));
    });
});
