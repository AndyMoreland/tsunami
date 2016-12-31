import * as ts from "typescript";
import { visitAllNodesInTree } from "./utilities/languageUtilities";

function whitespaceInsensitiveEquals(a: string, b: string): boolean {
    return a === b;
}

export function findIdenticalNodes(container: ts.Node, target: ts.Node): ts.Node[] {
    const matches: ts.Node[] = [];
    const targetText = target.getText();

    const visitNode = function(node: ts.Node) {
        if (whitespaceInsensitiveEquals(node.getText(), targetText)) {
            matches.push(node);
            return false;
        }

        return true;
    };

    visitAllNodesInTree(container, visitNode);

    return matches;
}
