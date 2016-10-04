import { ImportBlockFormatter } from "../src/imports/ImportBlockFormatter";
import * as Promise from "bluebird";
import { expect } from "chai";
import * as fs from "fs";
import * as ts from "typescript";
import * as path from "path";
import { ImportBlock, ImportRecords } from "../src/imports/ImportBlock";
import { ImportEditor } from "../src/imports/ImportEditor";
import { SimpleImportBlockFormatter } from "../src/imports/SimpleImportBlockFormatter";
import { applyCodeEditsInMemory, applyCodeEdits } from "../src/utilities/ioUtils";

declare function describe(foo: string, cb: Function): void;
declare function it(foo: string, cb: Function): void;

const readFilePromise = Promise.promisify(fs.readFile);

function getSourceFileFor(filename: string): ts.SourceFile {
    const buffer = fs.readFileSync(filename);
    return ts.createSourceFile(filename, buffer.toString(), ts.ScriptTarget.ES5, true);
}

function expectMatches(editor: ImportEditor, fileName: string): void {
    const sourceFile = getSourceFileFor(path.join(__dirname, "fixtures", `${fileName}.ts.pre`));
    const edits = editor.applyImportBlockToFile(sourceFile, ImportBlock.fromFile(sourceFile));
    const result = applyCodeEditsInMemory(sourceFile.getText(), edits);

    /* Account for trailing \n in fixture file */
    expect(result.trim()).to.eql(fs.readFileSync(path.join(__dirname, "fixtures", `${fileName}.ts.post`)).toString().trim());
}

describe("ImportEditor", () => {
    const editor = new ImportEditor(new SimpleImportBlockFormatter());

    it("should order global before scope before project imports",
       () => expectMatches(editor, "global-scope-project"));


    it("should not add extra whitespace when imports are split into two chunks by a non-import statement",
       () => expectMatches(editor, "split-imports"))
});
