import { expect } from "chai";
import { ImportBlockBuilder } from "../src/imports/ImportBlockBuilder";
import { ModuleSpecifier } from "../src/imports/ImportStatement";

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

    describe("initializing from an existing block", () => {
        const importBlock = ImportBlockBuilder.empty()
            .addImportBinding(typescriptModule, {symbolName: "SyntaxKind"}).build();

        const importBlock2 = ImportBlockBuilder.from(importBlock)
            .addImportBinding(typescriptModule, {symbolName: "LanguageService"}).build();

        const originalTypescriptRecords = importBlock.importRecords["typescript"].importClause.namedBindings;
        const newTypescriptRecords = importBlock2.importRecords["typescript"].importClause.namedBindings;

        it("should not modify the original", () => expect(originalTypescriptRecords).to.not.contain({symbolName: "LanguageService"}));
        it("should contain the original", () => expect(newTypescriptRecords).to.contain({symbolName: "SyntaxKind"}));
        it("should contain the new import", () => expect(newTypescriptRecords).to.contain({symbolName: "LanguageService"}));
    });
});
