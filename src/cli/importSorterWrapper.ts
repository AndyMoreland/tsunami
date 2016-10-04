/// <reference path="../../typings/node/node.d.ts" />

import * as fs from "fs";
import * as ts from "typescript";
import { ImportBlock } from "../imports/ImportBlock";
import { ImportEditor } from "../imports/ImportEditor";
import { SimpleImportBlockFormatter } from "../imports/SimpleImportBlockFormatter";
import { applyCodeEdits } from "../utilities/ioUtils";

function getSourceFileFor(filename: string): ts.SourceFile {
    return ts.createSourceFile(filename, fs.readFileSync(filename).toString(), ts.ScriptTarget.ES5, true);
}

process.argv.slice(2).forEach((filename) => {
    const editor = new ImportEditor(new SimpleImportBlockFormatter());
    const sourceFile = getSourceFileFor(filename);
    const importBlock = ImportBlock.fromFile(sourceFile);
    const edits = editor.applyImportBlockToFile(sourceFile, importBlock);

    applyCodeEdits(filename, edits)
        .then(() => console.log("Edited: ", filename));
});
