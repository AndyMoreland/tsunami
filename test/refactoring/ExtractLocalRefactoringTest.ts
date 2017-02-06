import { expect } from "chai";
import * as fs from "fs";
import * as path from "path";
import * as ts from "typescript";
import { ExtractLocalRefactoringProvider } from "../../src/refactoring/ExtractLocalRefactoring";

declare function describe(foo: string, cb: Function): void;
declare function it(foo: string, cb: Function): void;

function getSourceFileFor(filename: string): ts.SourceFile {
    const buffer = fs.readFileSync(filename);
    return ts.createSourceFile(filename, buffer.toString(), ts.ScriptTarget.ES5, true);
}

const PROVIDER = new ExtractLocalRefactoringProvider();

describe("ExtractLocalRefactoring", () => {
    it("should be pretty cool", async () => {
        const file = getSourceFileFor(
            path.join(__dirname, "..", "fixtures", "simple-extract-local.ts.pre")
        );

        const response = await PROVIDER.provideRefactorings(
            file,
            {
                start: 65,
                end: 66
            },
            null!
        );
    });
});
