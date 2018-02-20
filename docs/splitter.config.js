const path = require("path");
const fableUtils = require("fable-utils");

function resolve(filePath) {
    return path.resolve(__dirname, filePath)
}

function runScript(scriptPath) {
    try {
        console.log("Running script")
        var childProcess = require("child_process");
        var path = require("path");
        var cp = childProcess.fork(scriptPath);
        cp.on("exit", function (code, signal) {
            if (code === 0) {
                console.log("Success");
            } else {
                console.log("Exit", { code: code, signal: signal });
            }
        });
        cp.on("error", console.error.bind(console));
    } catch (err) {
        console.error(err);
    }
}

var isProduction = process.argv.indexOf("-p") >= 0;
console.log("[Environment]: " + (isProduction ? "production" : "development"));

var babelOptions = fableUtils.resolveBabelOptions({
    plugins: ["transform-es2015-modules-commonjs"]
});

module.exports = {
    entry: resolve("Docs.fsproj"),
    outDir: resolve("build"),
    babel: babelOptions,
    fable: { define: isProduction ? [] : ["DEBUG"], },
    postbuild() {
        runScript(resolve("build/src/Main.js"))
    }
};
