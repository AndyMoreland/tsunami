import { isBlockScopeBoundary, nodeContainsPoint } from "./utilities";
import * as ts from "typescript/lib/typescript";

export function getScopesContainingPoint(sourceFile: ts.SourceFile, point: number) {
    const results: ts.Node[] = [];

    const visitNode = (node: ts.Node) => {
        if (nodeContainsPoint(node, point)) {
            if (isBlockScopeBoundary(node)) {
                results.push(node);
            }

            ts.forEachChild(node, visitNode);
        }
    };
    ts.forEachChild(sourceFile, visitNode);

    return results;
}
