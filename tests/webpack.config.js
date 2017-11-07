var path = require('path');
var fs = require('fs');

function resolve(filePath) {
    return path.resolve(__dirname, filePath)
}

var babelOptions = {
    "presets": [
        [resolve("../node_modules/babel-preset-es2015"), {
            "modules": false
        }]
    ]
}

module.exports = {
    entry: resolve('./Fable.Json.Tests.fsproj'),
    output: {
        filename: 'tests.bundle.js',
        path: resolve('./bin'),
    },
    target: "node",
    module: {
        rules: [{
            test: /\.fs(x|proj)?$/,
            use: {
                loader: "fable-loader",
                options: {
                    babel: babelOptions
                }
            }
        }, {
            test: /\.js$/,
            exclude: /node_modules\/(?!fable)/,
            use: {
                loader: 'babel-loader',
                options: babelOptions
            },
        }]
    },
};