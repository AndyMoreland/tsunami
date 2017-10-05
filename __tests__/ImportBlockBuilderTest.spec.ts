import { ImportBlockBuilder } from "../src/imports/ImportBlockBuilder";
import { ModuleSpecifier } from "../src/imports/ImportStatement";

declare function describe(foo: string, cb: Function): void;
declare function it(foo: string, cb: Function): void;

function toModuleSpecifier(specifier: string): ModuleSpecifier {
    return specifier as ModuleSpecifier;
}

const TYPESCRIPT = toModuleSpecifier("typescript");
const LODASH = toModuleSpecifier("lodash");

const UA_SYNTAX_KIND = { symbolName: "SyntaxKind" };
const UA_LANGUAGE_SERVICE = { symbolName: "LanguageService" };

describe("ImportBlockBuilder", () => {
    it("should be able to add a simple symbol import", () => {
        const importBlock = ImportBlockBuilder.empty()
            .addImportBinding(TYPESCRIPT, UA_SYNTAX_KIND).build();
        expect(importBlock.importRecords).toContain.keys(TYPESCRIPT);
        expect(importBlock.importRecords["typescript"].importClause.namedBindings).toContain({
            symbolName: "SyntaxKind"
        });
    });

    describe("adding default imports", () => {
        const importBlock = ImportBlockBuilder.empty()
            .addDefaultImport(TYPESCRIPT, "default_alias").build();

        it("should contain an import record for the module", () => expect(importBlock.importRecords).toContain.keys(TYPESCRIPT));

        describe("the module record", () => {
            const record = importBlock.importRecords[TYPESCRIPT];
            it("should not add a named binding", () => expect(record.importClause.namedBindings).toBe.empty);
            it("should store the default name", () => expect(record.importClause.defaultName).toEqual("default_alias"));
        });
    });

    describe("using namespace imports", () => {
        const importBlock = ImportBlockBuilder.empty()
            .addNamespaceSpecifier(TYPESCRIPT, "ts").build();

        const specifier = importBlock.importRecords[TYPESCRIPT].namespaceImport;

        it("should add an import record", () => expect(importBlock.importRecords).toContain.keys(TYPESCRIPT));
        it("should add the namespace specifier", () => expect(specifier).not.toBe(null));
        it("should record the namespace specifier alias", () => expect(specifier!.alias).toEqual("ts"));
    });

    describe("initializing from an existing block", () => {
        const importBlock = ImportBlockBuilder.empty()
            .addImportBinding(TYPESCRIPT, UA_SYNTAX_KIND).build();

        const importBlock2 = ImportBlockBuilder.from(importBlock)
            .addImportBinding(TYPESCRIPT, UA_LANGUAGE_SERVICE).build();

        const originalTypescriptRecords = importBlock.importRecords[TYPESCRIPT].importClause.namedBindings;
        const newTypescriptRecords = importBlock2.importRecords[TYPESCRIPT].importClause.namedBindings;

        it("should not modify the original", () => expect(originalTypescriptRecords).not.toContain(UA_LANGUAGE_SERVICE));
        it("should contain the original", () => expect(newTypescriptRecords).toContain(UA_SYNTAX_KIND));
        it("should contain the new import", () => expect(newTypescriptRecords).toContain(UA_LANGUAGE_SERVICE));
    });

    describe("removing the only import", () => {
        const importBlock = ImportBlockBuilder.empty()
            .addImportBinding(TYPESCRIPT, UA_SYNTAX_KIND)
            .withoutImport(TYPESCRIPT, "SyntaxKind")
            .build();

        it("should remove the import record", () => expect(importBlock.importRecords).not.toContain.keys(TYPESCRIPT));
    });

    describe("it should be able to rename a module", () => {
        const importBlock = ImportBlockBuilder.empty()
            .addImportBinding(TYPESCRIPT, UA_SYNTAX_KIND)
            .renameModule(TYPESCRIPT, LODASH)
            .build();

        it("should rename the import record", () => {
            expect(importBlock.importRecords).not.toContain.keys(TYPESCRIPT);
            expect(importBlock.importRecords).toContain.keys(LODASH);
        });
    });
});
