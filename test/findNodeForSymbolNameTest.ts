import { findNodeForSymbolName } from "../src/utilities/moveSymbolUtils";
import { expect } from "chai";
import * as fs from "fs";
import * as ts from "typescript";
import * as path from "path";

declare function describe(foo: string, cb: Function): void;
declare function it(foo: string, cb: Function): void;

function getSourceFileFor(filename: string): ts.SourceFile {
    const buffer = fs.readFileSync(filename);
    return ts.createSourceFile(filename, buffer.toString(), ts.ScriptTarget.ES5, true);
}

describe("findNodeForSymbolName", () => {
    const fileWithExportedSybmols = getSourceFileFor(path.join(
        __dirname,
        "fixtures",
        `file-with-exported-symbols.ts`
    ));

    const exportedFunctionNode = findNodeForSymbolName(fileWithExportedSybmols, "exportedFunction");

    it("should find exported function nodes",
       () => expect(exportedFunctionNode).to.exist);
});
