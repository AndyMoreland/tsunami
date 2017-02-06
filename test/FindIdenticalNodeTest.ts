import { findIdenticalNodes } from "../src/FindIdenticalNodes";
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

describe("findIdenticalNodes, ignoring whitespace", () => {
    const sourceFile = getSourceFileFor(path.join(__dirname, "fixtures", "simple-extract-local.ts.pre"));

    it ("Should find nodes that differ only in newlines", () => {
        const fnNode = findNodeMatching(sourceFile, "foo", ts.SyntaxKind.FunctionDeclaration);
        const firstMapNode = findNodeMatching(sourceFile, "map", ts.SyntaxKind.ExpressionStatement);

        expect(fnNode).to.exist;
        expect(firstMapNode).to.exist;

        const allNodes = findIdenticalNodes(fnNode!, firstMapNode!);

        expect(allNodes.length).to.eql(2);
    });
});
