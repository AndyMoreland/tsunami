import * as Bluebird from "bluebird";
import * as fs from "fs";
import * as ts from "typescript";
import { ImportBlockBuilder } from "../imports/ImportBlockBuilder";
import { ImportEditor } from "../imports/ImportEditor";
import { ModuleSpecifier } from "../imports/ImportStatement";
import { SimpleImportBlockFormatter } from "../imports/SimpleImportBlockFormatter";
import { applyCodeEdits } from "../utilities/ioUtils";

const readFilePromise = Bluebird.promisify(fs.readFile);

function getSourceFileFor(filename: string): Promise<ts.SourceFile> {
    return Promise.resolve(readFilePromise(filename)).then(buffer => {
        return ts.createSourceFile(
            filename,
            buffer.toString(),
            ts.ScriptTarget.ES5,
            true
        );
    });
}

process.argv.slice(2).forEach(filename => {
    const editor = new ImportEditor(
        SimpleImportBlockFormatter.withDefaultOptions()
    );
    getSourceFileFor(filename).then(sourceFile => {
        const importBlock = ImportBlockBuilder.fromFile(sourceFile)
            .addNamespaceSpecifier("react" as ModuleSpecifier, "React")
            .build();
        const edits = editor.applyImportBlockToFile(sourceFile, importBlock);

        applyCodeEdits(filename, edits).then(() =>
            console.log("Edited: ", filename)
        );
    });
});
