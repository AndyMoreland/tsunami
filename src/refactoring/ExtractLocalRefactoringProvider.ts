import * as ts from "typescript";
import { RegionSpan } from "../protocol/types";
import { replaceNode } from "../utilities/editUtils";
import { getNodesContainingPoint } from "../utilities/languageUtilities";
import { TsunamiContext } from "../Context";
import { getExpressionsContainingPoint } from "../ExpressionTree";
import { findIdenticalNodes } from "../FindIdenticalNodes";
import { getScopesContainingPoint } from "../ScopeTree";
import { RefactoringOption, RefactoringResult } from "./RefactoringOption";
import { RefactoringContext } from "./RefactoringContext";
import { RefactoringProvider, RefactoringProviderResult } from "./RefactoringProvider";

export class ExtractLocalRefactoring implements RefactoringOption {
    constructor(
        private document: ts.SourceFile,
        private target: number
    ) {}

    get label(): string {
        return "Extract Local...";
    }

    async execute(context: RefactoringContext, globalContext: TsunamiContext): Promise<RefactoringResult> {
        const expressions = getExpressionsContainingPoint(this.document, this.target);
        const scopes = getScopesContainingPoint(this.document, this.target);

        const expression = await context.promptChoice([]); // FIXME

        if (!expression) {
            return RefactoringResult.empty();
        }

        const scope = await context.promptChoice([]) // FIXME;

        if (!scope) {
            return RefactoringResult.empty();
        }

        const newName = await context.promptForText("New Name") || "extractedVariable";
        const insertion = null; // FIXME
        const replaces = findIdenticalNodes(scope.data, expression).map(node => replaceNode(this.document, node, newName));

        return {
            localEdits: replaces
        };
    }
}

export class ExtractLocalRefactoringProvider implements RefactoringProvider {
    constructor() {}

    async provideRefactorings(document: ts.SourceFile, range: RegionSpan, context: TsunamiContext): Promise<RefactoringProviderResult> {
        const expressions = getExpressionsContainingPoint(document, range.start);

        if (expressions.length === 0) {
            return {
                refactorings: []
            }
        }

        return {
            refactorings: [new ExtractLocalRefactoring(document, range.start)]
        };
    }
}
