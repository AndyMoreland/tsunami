import { expect } from "chai";
import * as fs from "fs";
import * as path from "path";
import * as ts from "typescript";
import { ImportBlockBuilder } from "../src/imports/ImportBlockBuilder";
import { ImportBlockFormatter } from "../src/imports/ImportBlockFormatter";
import { SimpleImportBlockFormatter } from "../src/imports/SimpleImportBlockFormatter";

declare function describe(foo: string, cb: Function): void;
declare function it(foo: string, cb: Function): void;

function getSourceFileFor(filename: string): ts.SourceFile {
    return ts.createSourceFile(filename, fs.readFileSync(filename).toString(), ts.ScriptTarget.ES5, true);
}

function expectMatches(formatter: ImportBlockFormatter, fileName: string): void {
    const sourceFile = getSourceFileFor(path.join(__dirname, "fixtures", `${fileName}.ts.pre`));
    const result = formatter.formatImportBlock(path.join(__dirname, "fixtures"), ImportBlockBuilder.fromFile(sourceFile).build());

    /* Account for trailing \n in fixture file */
    expect(result.trim()).to.eql(fs.readFileSync(path.join(__dirname, "fixtures", `${fileName}.ts.post`)).toString().trim());
}

describe("SimpleImportBlockFormatter", () => {
    it("should order global before scope before project imports",
       () => expectMatches(SimpleImportBlockFormatter.withDefaultOptions(), "global-scope-project"));

    it("should order farthest-to-closest in project imports",
       () => expectMatches(SimpleImportBlockFormatter.withDefaultOptions(), "farthest-to-closest"));

    it("should preserve multi-line imports properly",
       () => expectMatches(SimpleImportBlockFormatter.withDefaultOptions(), "multiLine"));

    it("should replace single-quotes in module specifiers with double",
       () => expectMatches(SimpleImportBlockFormatter.withDefaultOptions(), "moduleSpecifierQuotes"));

    it("should sort named bindings alphabetically",
       () => expectMatches(SimpleImportBlockFormatter.withDefaultOptions(), "alphabetical-bindings"));

    it("should collapse multiple import statements from the same module",
       () => expectMatches(SimpleImportBlockFormatter.withDefaultOptions(), "multiple-imports"));

    it("should properly rewrite aliased default imports",
       () => expectMatches(SimpleImportBlockFormatter.withDefaultOptions(), "default-imports"));

    it("should do nothing to files without imports",
       () => expectMatches(SimpleImportBlockFormatter.withDefaultOptions(), "no-imports"));
});
