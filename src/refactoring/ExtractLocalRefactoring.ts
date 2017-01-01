import * as ts from "typescript";
import { RegionSpan } from "../protocol/types";
import { TsunamiContext } from "../Context";
import { getExpressionsContainingPoint } from "../ExpressionTree";
import { getScopesContainingPoint } from "../ScopeTree";
import { Refactoring, RefactoringResult } from "./Refactoring";
import { RefactoringContext } from "./RefactoringContext";
import { RefactoringProvider, RefactoringProviderResult } from "./RefactoringProvider";

function getChoiceTextForNode(node: ts.Node): string {
    const fullText = node.getText();


}

export class ExtractLocalRefactoring implements Refactoring {
    label = "Extract Local...";

    constructor(
        private document: ts.SourceFile,
        private target: number
    ) {}

    async execute(
        context: RefactoringContext,
        globalContext: TsunamiContext
    ): Promise<RefactoringResult> {
        const expressions = getExpressionsContainingPoint(this.document, this.target);
        const scopes = getScopesContainingPoint(this.document, this.target,)

        if (expressions.length === 0 || scopes.length === 0) {
            return RefactoringResult.empty();
        }

        const expression = await context.promptChoice(expressions);
        const scope = await context.promptChoice(scopes);


    }
}

export class ExtractLocalRefactoringProvider implements RefactoringProvider {
    async provideRefactorings(
        document: ts.SourceFile,
        range: RegionSpan,
        context: TsunamiContext
    ): Promise<RefactoringProviderResult> {
        const expressions = getExpressionsContainingPoint(document, range.start);

        if (expressions.length === 0) {
            return {
                refactorings: []
            };
        }

        return {
            refactorings: [new ExtractLocalRefactoring(document, range.start)]
        }
    }
}
