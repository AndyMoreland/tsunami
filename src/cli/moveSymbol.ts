import * as Promise from "bluebird";
import * as fs from "fs";
import * as path from "path";
import * as ts from "typescript";
import { ImportBlock } from "../imports/ImportBlock";
import { ImportEditor } from "../imports/ImportEditor";
import { SimpleImportBlockFormatter } from "../imports/SimpleImportBlockFormatter";
import { applyCodeEdits } from "../utilities/ioUtils";
import { Tsunami } from "../Tsunami";
import { TsProject } from "../tsProject";

const readFilePromise = Promise.promisify(fs.readFile);

function getSourceFileFor(filename: string): Promise<ts.SourceFile> {
    return readFilePromise(filename).then(buffer => {
        return ts.createSourceFile(filename, buffer.toString(), ts.ScriptTarget.ES5, true);
    });
}

const [root, from, to, symbolName] = process.argv.slice(2);

const editor = new ImportEditor(new SimpleImportBlockFormatter());
const project = new TsProject(root, path.join(root, "tsconfig.json"));

project.getFileNames().then(filenames => {
    filenames.forEach(filename => {
        getSourceFileFor(filename).then(sourceFile => {
            const importBlock = ImportBlock.fromFile(sourceFile);
            const edits = editor.applyImportBlockToFile(sourceFile, importBlock);

            applyCodeEdits(filename, edits)
                .then(() => console.log("Edited: ", filename));
        });
    });
});
