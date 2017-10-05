import { ImportBlockBuilder } from "../src/imports/ImportBlockBuilder";
import * as fs from "fs";
import * as ts from "typescript";
import * as path from "path";
import { ImportEditor } from "../src/imports/ImportEditor";
import { SimpleImportBlockFormatter } from "../src/imports/SimpleImportBlockFormatter";
import { applyCodeEditsInMemory } from "../src/utilities/ioUtils";

declare function describe(foo: string, cb: Function): void;
declare function it(foo: string, cb: Function): void;

function getSourceFileFor(filename: string): ts.SourceFile {
    const buffer = fs.readFileSync(filename);
    return ts.createSourceFile(filename, buffer.toString(), ts.ScriptTarget.ES5, true);
}

function expectMatches(editor: ImportEditor, fileName: string): void {
    const sourceFile = getSourceFileFor(path.join(__dirname, "fixtures", `${fileName}.ts.pre`));
    const edits = editor.applyImportBlockToFile(sourceFile, ImportBlockBuilder.fromFile(sourceFile).build());
    const result = applyCodeEditsInMemory(sourceFile.getFullText(), edits);

    /* Account for trailing \n in fixture file */
    expect(result.trim()).toEqual(fs.readFileSync(path.join(__dirname, "fixtures", `${fileName}.ts.post`)).toString().trim());
}

describe("ImportEditor", () => {
    const editor = new ImportEditor(SimpleImportBlockFormatter.withDefaultOptions());

    it("should order global before scope before project imports",
       () => expectMatches(editor, "global-scope-project"));

    it("should not add extra whitespace when imports are split into two chunks by a non-import statement",
       () => expectMatches(editor, "split-imports"));

    it("should not munch comments that follow the import block",
       () => expectMatches(editor, "imports-followed-by-comments"));

    it("should not screw with leading comments",
       () => expectMatches(editor, "leading-comment"));
});
