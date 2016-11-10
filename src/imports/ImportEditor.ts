import * as path from "path";
import * as ts from "typescript";
import { CodeEdit, Extent } from "../protocol/types";
import { convertPositionToLocation } from "../utilities/languageUtilities";
import { ImportBlock } from "./ImportBlock";
import { ImportBlockFormatter } from "./ImportBlockFormatter";

export class ImportEditor {
    constructor(
        private formatter: ImportBlockFormatter
    ) {}

    private getImportExtents(sourceFile: ts.SourceFile): Extent[] {
        const results: Extent[] = [];
        let inExtent = false;
        let startOfExtent = -1;
        let endOfExtent = -1;

        ts.forEachChild(sourceFile, node => {
            if (node.kind === ts.SyntaxKind.ImportDeclaration) {
                if (!inExtent) {
                    inExtent = true;
                    startOfExtent = node.getStart();
                }
                endOfExtent = node.getEnd();
            } else {
                if (inExtent) {
                    inExtent = false;
                    results.push({
                        start: convertPositionToLocation(sourceFile, startOfExtent),
                        end: convertPositionToLocation(sourceFile, endOfExtent + 1)
                    });
                }
            }
        });

        return results.length > 0 ? results : [
            {
                start: {
                    line: 1,
                    offset: 1
                },
                end: {
                    line: 1,
                    offset: 1
                }
            }
        ];
    }

    public applyImportBlockToFile(sourceFile: ts.SourceFile, importBlock: ImportBlock): CodeEdit[] {
        const importExtents = this.getImportExtents(sourceFile);

        if (importExtents.length === 0) {
            return [];
        }

        const firstEdit: CodeEdit = {
            start: importExtents[0].start,
            end: importExtents[0].end,
            newText: this.formatter.formatImportBlock(path.dirname(sourceFile.fileName), importBlock) + "\n"
        };

        const deletions: CodeEdit[] = importExtents.slice(1).map(extent => {
            return Object.assign({}, extent, {newText: ""}) as CodeEdit;
        });

        return [firstEdit, ...deletions];
    }
}
