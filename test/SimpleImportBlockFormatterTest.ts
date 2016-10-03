import { expect } from "chai";
import * as fs from "fs";
import * as path from "path";
import * as ts from "typescript";
import { ImportBlock } from "../src/imports/ImportBlock";
import { ImportBlockFormatter } from "../src/imports/ImportBlockFormatter";
import { SimpleImportBlockFormatter } from "../src/imports/SimpleImportBlockFormatter";

declare function describe(foo: string, cb: Function): void;
declare function it(foo: string, cb: Function): void;

function getSourceFileFor(filename: string): ts.SourceFile {
    return ts.createSourceFile(filename, fs.readFileSync(filename).toString(), ts.ScriptTarget.ES5, true);
}

function expectMatches(formatter: ImportBlockFormatter, fileName: string): void {
    const sourceFile = getSourceFileFor(path.join(__dirname, "fixtures", `${fileName}.ts.pre`));
    const result = formatter.formatImportBlock(path.join(__dirname, "fixtures"), ImportBlock.fromFile(sourceFile));

    /* Account for trailing \n in fixture file */
    expect(result + "\n").to.eql(fs.readFileSync(path.join(__dirname, "fixtures", `${fileName}.ts.post`)).toString());
}

describe("SimpleImportBlockFormatter", () => {
    it("should order global before scope before project imports",
       () => expectMatches(new SimpleImportBlockFormatter(), "global-scope-project"));

    it("should order farthest-to-closest in project imports",
       () => expectMatches(new SimpleImportBlockFormatter(), "farthest-to-closest"));

    it("should preserve multi-line imports properly",
       () => expectMatches(new SimpleImportBlockFormatter(), "multiLine"));

    it("should replace single-quotes in module specifiers with double",
       () => expectMatches(new SimpleImportBlockFormatter(), "moduleSpecifierQuotes"));

    it("should sort named bindings alphabetically",
       () => expectMatches(new SimpleImportBlockFormatter(), "alphabetical-bindings"));

    it("should collapse multiple import statements from the same module",
       () => expectMatches(new SimpleImportBlockFormatter(), "multiple-imports"));
});
