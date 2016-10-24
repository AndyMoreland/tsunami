import { convertPositionToLocation } from "./languageUtilities";
import { CodeEdit } from "../protocol/types";
import * as ts from "typescript";

export function withoutNode(sourceFile: ts.SourceFile, node: ts.Node): CodeEdit {
    return {
        start: convertPositionToLocation(sourceFile, node.getStart()),
        end: convertPositionToLocation(sourceFile, node.getEnd()),
        newText: ""
    };
}

export function appendNode(sourceFile: ts.SourceFile, node: ts.Node): CodeEdit {
    const newText = node.getText();

    return {
        start: convertPositionToLocation(sourceFile, sourceFile.getEnd()),
        end: convertPositionToLocation(sourceFile,  sourceFile.getEnd()),
        newText
    };
}
