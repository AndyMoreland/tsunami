import { isBlockScopeBoundary, nodeContainsPoint } from "./utilities/languageUtilities";
import * as ts from "typescript/lib/typescript";

export function getScopesContainingPoint(sourceFile: ts.SourceFile, point: number) {
    const results: ts.Node[] = [];

    function visitNode(node: ts.Node) {
        if (nodeContainsPoint(node, point)) {
            if (isBlockScopeBoundary(node)) {
                results.push(node);
            }

            ts.forEachChild(node, visitNode);
        }
    }
    ts.forEachChild(sourceFile, visitNode);

    return results;
}
