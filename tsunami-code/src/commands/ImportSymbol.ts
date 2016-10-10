
export async function importSymbolCommand(editor, edit, symbol?: string) {
        // The code you place here will be executed every time your command is executed
        console.log("completion seeded with: ", symbol);
        const context = tsunami.getContext();
        let results: CompletionItem[] = [];
        const exactMatchResults: CompletionItem[] = [];

        context.fileIndexerMap.forEach((indexer, file) => {
            const definitions = indexer.getDefinitionIndex();
            Object.keys(definitions).forEach(k => {
                const def = definitions[k];
                const item = {
                    definition: def,
                    label: def.text,
                    description: ""
                };
                results.push(item);

                if (symbol && def.text === symbol) {
                    exactMatchResults.push(item);
                }
            });
        });

        if (results.length === 0) {
            vscode.window.showErrorMessage("No matching symbols found in project.");
            return;
        }

        if (exactMatchResults.length > 0) {
            results = exactMatchResults;
        }

        const choice = results.length === 1 ? results[0] : await vscode.window.showQuickPick(results);
        const sourceFile = ts.createSourceFile(editor.document.fileName, editor.document.getText(), ts.ScriptTarget.ES5, true);
        const newBlock = tsu.ImportBlockBuilder.from(tsu.ImportBlock.fromFile(sourceFile))
            .addImportBinding(choice.definition.moduleSpecifier.replace(/\.tsx?/g, "") as any,
                              { symbolName: choice.definition.text }
            ).build();
        const edits = (new tsu.ImportEditor(new tsu.SimpleImportBlockFormatter()))
            .applyImportBlockToFile(sourceFile, newBlock);
        editor.edit((editBuilder) => applyCodeEdit(editBuilder, edits[0]));

    }
