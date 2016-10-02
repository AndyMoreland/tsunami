import { isExpression, nodeContainsPoint } from "./utilities/languageUtilities";
import * as ts from "typescript/lib/typescript";

export function getExpressionsContainingPoint(sourceFile: ts.SourceFile, point: number) {
    const results: ts.Node[] = [];

    function visitNode(node: ts.Node) {
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
