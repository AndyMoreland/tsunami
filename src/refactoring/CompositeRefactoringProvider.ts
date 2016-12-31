import * as ts from "typescript";
import { RegionSpan } from "../protocol/types";
import { TsunamiContext } from "../Context";
import { Refactoring } from "./Refactoring";
import { RefactoringProvider, RefactoringProviderResult } from "./RefactoringProvider";

export class CompositeRefactoringProvider implements RefactoringProvider {
    constructor(private providers: RefactoringProvider[]) {}

    async provideRefactorings(
        document: ts.SourceFile,
        span: RegionSpan,
        context: TsunamiContext
    ): Promise<RefactoringProviderResult> {
        const refactorings: Refactoring[] = [];

        await this.providers.map(async provider => {
            const result = await provider.provideRefactorings(document, span, context);

            refactorings.push(...result.refactorings);
        });

        return { refactorings };
    }
}
