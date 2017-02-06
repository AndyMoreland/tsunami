import * as ts from "typescript";
import { CodeEdit } from "./protocol/types";

export interface FileEditor {
    commit(document: ts.SourceFile): CodeEdit[];
}
