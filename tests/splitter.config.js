module.exports = {
    entry: resolve("./Thoth.Tests.fsproj"),
    outDir: resolve("./bin"),
    babel: {
        plugins: ["transform-es2015-modules-commonjs"],
    },
    allFiles: true
};

function resolve(path) {
    return require("path").join(__dirname, path);
}
