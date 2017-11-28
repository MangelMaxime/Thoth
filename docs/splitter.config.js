/// @ts-check
const path = require("path");
const fableUtils = require("fable-utils");

function resolve(filePath) {
  return path.resolve(__dirname, filePath)
}

function runScript(scriptPath) {
  var scriptDir = path.dirname(scriptPath);
  // Delete files in directory from require cache
  Object.keys(require.cache).forEach(function(key) {
    if (key.startsWith(scriptDir))
      delete require.cache[key]
  })
  require(scriptPath);
}

var outFile = resolve("build/Main.js");

var babelOptions = fableUtils.resolveBabelOptions({
    plugins: ["transform-es2015-modules-commonjs"]
});

module.exports = {
  entry: resolve("src/Fable.StaticPageGenerator.fsproj"),
  outDir: path.dirname(outFile),
  babel: babelOptions,
  fable: { define: ["DEBUG"] },
  postbuild() { runScript(outFile) }
};
