import { ImportStatementType, ModuleSpecifier } from "../src/imports/ImportStatement";
import { ImportBlockBuilder } from "../src/imports/ImportBlock";
import { expect } from "chai";

declare function describe(foo: string, cb: Function): void;
declare function it(foo: string, cb: Function): void;

function toModuleSpecifier(specifier: string): ModuleSpecifier {
    return specifier as ModuleSpecifier;
}

const typescriptModule = toModuleSpecifier("typescript");

describe("ImportBlockBuilder", () => {
    it("should be able to add a simple import", () => {
        const importBlock = ImportBlockBuilder.empty()
            .addImportBinding(typescriptModule, {symbolName: "SyntaxKind"}).build();
        expect(importBlock.importRecords).to.contain.keys("typescript");
        expect(importBlock.importRecords["typescript"].importClause.namedBindings).to.contain({
            symbolName: "SyntaxKind"
        });
    });

    it("should be able to initialize itself from a block", () => {
        const importBlock = ImportBlockBuilder.empty()
            .addImportBinding(typescriptModule, {symbolName: "SyntaxKind"}).build();

        const importBlock2 = ImportBlockBuilder.from(importBlock)
            .addImportBinding(typescriptModule, {symbolName: "LanguageService"}).build();

        const originalTypescriptRecords = importBlock.importRecords["typescript"].importClause.namedBindings;
        const newTypescriptRecords = importBlock2.importRecords["typescript"].importClause.namedBindings;

        expect(originalTypescriptRecords).to.not.contain({symbolName: "LanguageService"});
        expect(newTypescriptRecords).to.contain({symbolName: "SyntaxKind"});
        expect(newTypescriptRecords).to.contain({symbolName: "LanguageService"});
    });

});
