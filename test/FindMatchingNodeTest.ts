import { findNodeMatching } from "../src/utilities/languageUtilities";
import { expect } from "chai";
import * as fs from "fs";
import * as path from "path";
import * as ts from "typescript";

declare function describe(foo: string, cb: Function): void;
declare function it(foo: string, cb: Function): void;

function getSourceFileFor(filename: string): ts.SourceFile {
    const buffer = fs.readFileSync(filename);
    return ts.createSourceFile(filename, buffer.toString(), ts.ScriptTarget.ES5, true);
}

describe("FindMatchingNode", () => {
    const sourceFile = getSourceFileFor(path.join(__dirname, "fixtures", "simple-extract-local.ts.pre"));
    describe("when finding matching nodes", () => {
        it("should find a matching top-level node", () => {
            const functionNode = findNodeMatching(sourceFile, "function", ts.SyntaxKind.FunctionDeclaration);
            expect(functionNode).to.exist;
        });

        it("should find a matching nested node", () => {
            const expressionNode = findNodeMatching(sourceFile, "x.map", ts.SyntaxKind.ExpressionStatement);
            expect(expressionNode).to.exist;
        });
    });

    describe("when finding nodes with no matches", () => {
        it("should find no nodes", () => {
            const functionNode = findNodeMatching(sourceFile, "fish", ts.SyntaxKind.FunctionDeclaration);
            expect(functionNode).to.not.exist;
        });
    });
});
