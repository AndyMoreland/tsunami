import * as ts from "typescript";
import { RegionSpan } from "../protocol/types";
import { TsunamiContext } from "../Context";
import { RefactoringOption } from "./RefactoringOption";

export interface RefactoringProviderResult {
    refactorings: RefactoringOption[];
}

export interface RefactoringProvider {
    provideRefactorings(document: ts.SourceFile, range: RegionSpan, context: TsunamiContext): Promise<RefactoringProviderResult>;
}
