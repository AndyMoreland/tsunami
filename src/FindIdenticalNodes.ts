import * as ts from "typescript";
import { visitAllNodesInTree } from "./utilities/languageUtilities";

function whitespaceInsensitiveEquals(a: string, b: string): boolean {
    return a.replace(/\s*/g, "") === b.replace(/\s*/g, "");
}

export function findIdenticalNodes(container: ts.Node, target: ts.Node): ts.Node[] {
    const matches: ts.Node[] = [];
    const targetText = target.getText();

    const visitNode = function(node: ts.Node) {
        if (whitespaceInsensitiveEquals(targetText, node.getText())) {
            matches.push(node);
        }

        return true;
    };

    visitAllNodesInTree(container, visitNode);

    return matches;
}
