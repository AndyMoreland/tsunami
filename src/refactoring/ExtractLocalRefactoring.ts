import * as ts from "typescript";
import { CodeEdit, RegionSpan } from "../protocol/types";
import { replaceNode } from "../utilities/editUtils";
import { TsunamiContext } from "../Context";
import { getExpressionsContainingPoint } from "../ExpressionTree";
import { FileEditor } from "../FileEditor";
import { findIdenticalNodes } from "../FindIdenticalNodes";
import { getScopesContainingPoint } from "../ScopeTree";
import { RefactoringContext } from "./RefactoringContext";
import { RefactoringOption, RefactoringResult } from "./RefactoringOption";
import { RefactoringProvider, RefactoringProviderResult } from "./RefactoringProvider";

function getChoiceTextForNode(node: ts.Node): string {
    let result = node.getText();

    const opening = result.indexOf("{");
    const closing = result.lastIndexOf("}");

    if (opening >= 0 && closing >= 0) {
        result = result.substring(0, opening + 1) + " ... " + result.substring(closing);
    }

    return result;
}

export class ExtractLocalRefactoring implements FileEditor {
    static for(scope: ts.Node, expression: ts.Node) {
        return new ExtractLocalRefactoring(scope, expression);
    }

    constructor(
        private scope: ts.Node,
        private expression: ts.Node
    ) { }

    commit(document: ts.SourceFile): CodeEdit[] {
        const matchingNodes = findIdenticalNodes(this.scope, this.expression);
        return matchingNodes.map(n => replaceNode(document, n, "newVariable"));
    }
}

export class ExtractLocalRefactoringOption implements RefactoringOption {
    label = "Extract Local...";

    constructor(
        private document: ts.SourceFile,
        private target: number
    ) { }

    async execute(
        context: RefactoringContext,
        globalContext: TsunamiContext
    ): Promise<RefactoringResult> {
        const expressions = getExpressionsContainingPoint(this.document, this.target);
        const scopes = getScopesContainingPoint(this.document, this.target);

        if (expressions.length === 0 || scopes.length === 0) {
            return RefactoringResult.empty();
        }

        const expression = await context.promptChoice(expressions.map(e => {
            return {
                id: "" + Math.random(),
                label: getChoiceTextForNode(e),
                data: e
            };
        }));

        if (!expression) {
            return RefactoringResult.empty();
        }

        const scope = await context.promptChoice(scopes.map(s => {
            return {
                id: "" + Math.random(),
                label: getChoiceTextForNode(s),
                data: s
            };
        }));

        if (!scope) {
            return RefactoringResult.empty();
        }


        return {
            localEdits: ExtractLocalRefactoring.for(scope.data, expression.data).commit(this.document)
        };
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
            refactorings: [new ExtractLocalRefactoringOption(document, range.start)]
        }
    }
}
