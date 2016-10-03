import * as fs from "fs";
import * as path from "path";
import * as ts from "typescript";
import { ImportBlock } from "../src/imports/ImportBlock";
import { SimpleImportBlockFormatter } from "../src/imports/SimpleImportBlockFormatter";

declare function describe(foo: string, cb: Function): void;
declare function it(foo: string, cb: Function): void;

function getSourceFileFor(filename: string): ts.SourceFile {
    return ts.createSourceFile(filename, fs.readFileSync(filename).toString(), ts.ScriptTarget.ES5, true);
}

describe("SimpleImportBlockFormatter", () => {
    const formatter = new SimpleImportBlockFormatter();
    const sourceFile = getSourceFileFor(path.join(__dirname, "fixtures", "a.ts.pre"));
    const result = formatter.formatImportBlock(__dirname, ImportBlock.fromFile(sourceFile));

    it("should have a good time", () => {
        console.log(result);
    });
});
