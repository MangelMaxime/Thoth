The sample for building VSCode extensions using F# and Fable.

### Requirements

 * VSCode
 * Node.js
 * .Net Framework or mono
 * `dotnet` 2.0
 * Yarn
 * ionide

### How to build

1. `build.cmd` on windows or `build.sh` on Linux/Mac
2. `code .`
3. Press `F5` for single build, or run `Watch` task and `Launch Only` debug configuration for watch mode compilation.

### How to release

1. `npm install -g vsce` (only needed once)
2. Create new entry in `RELEASE_NOTES.md`
3. Run `build.cmd Release`