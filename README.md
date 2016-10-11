How to Use From VSCode:
==============

1. Install `tsunami-code` from the vscode extension market
2. Open a typescript project.
3. Run `(tsu) Import Symbol`
4. Enjoy!


How to Use From The CLI:
==============

1. Run `npm install -g @derander/tsunami`
2. Use `tsunami-imports aFile.ts anotherFile.tsx aThirdFile.ts 'src/\*\*/\*.ts'`
3. Enjoy!


About:
==========

Tsunami is a TypeScript analysis project built on top of the typescript compiler's interfaces. Features:

0. Auto-Import

Tsunami indexes the `exports` of the project it's executed in (as well as the exports of its dependencies) in order
to determine the symbols that are available for import. It is capable of providing `CodeEdit`s that import these symbols.

1. Move Symbol refactoring

Tsunami can re-write all imports in a project to reflect a symbols move from `ModuleA` to `ModuleB`. For example, it can rewrite all instances of:

```
import { a } from "../foo";
```

to

```
import { a } from "../../bar";
```

while respecting relative paths properly.

2. Import Formatting (coalescing, ordering)


3. Export indexing

Tsunami can answer the question "what symbols are available for import?" for arbitrary typescript modules.
