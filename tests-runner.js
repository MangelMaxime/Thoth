const chokidar = require("chokidar");
const path = require("path");

const resolve = (filePath) => {
    return path.join(__dirname, filePath);
}

const watcher = chokidar.watch(
    [
        resolve("./src"),
        resolve("./tests")
    ],
    {
        ignoreInitial: true,
    }
);

const runMocha = () => {
    console.log("run mocha");
}

watcher
    .on("add", runMocha)
    .on("change", runMocha);
