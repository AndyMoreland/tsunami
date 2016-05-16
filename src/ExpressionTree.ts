
import { isExpression } from "./utilities";
import * as ts from "typescript/lib/typescript";
import { logSync } from "./log";

function nodeContainsPoint(node: ts.Node, point: number) {
    return node.getStart() <= point && node.getEnd() >= point;
}

export function getExpressionsContainingPoint(sourceFile: ts.SourceFile, point: number) {
    logSync("I've been invoked!");
    const results: ts.Node[] = [];

    const visitNode = (node: ts.Node) => {
         if (nodeContainsPoint(node, point)) {
            if (isExpression(node)) {
                results.push(node);
            }

            ts.forEachChild(node, visitNode);
        }
    };
    ts.forEachChild(sourceFile, visitNode);

    return results;
}
