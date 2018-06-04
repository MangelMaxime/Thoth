const path = require("path");
const webpack = require("webpack");
const fableUtils = require("fable-utils");

function resolve(filePath) {
    return path.join(__dirname, filePath)
}

var babelOptions = fableUtils.resolveBabelOptions({
    presets: [
        ["env", {
            "targets": {
                "browsers": ["last 2 versions"]
            },
            "modules": false
        }]
    ]
});

var isProduction = process.argv.indexOf("-p") >= 0;
console.log("Bundling for " + (isProduction ? "production" : "development") + "...");

var commonPlugins = [
];

module.exports = {
    devtool: undefined,
    entry: isProduction ? // We don't use the same entry for dev and production, to make HMR over style quicker for dev env
        {
            // We don't output style because it's already included by the docs
            demo: [
                "babel-polyfill",
                resolve('./Thoth.Elmish.Demo.fsproj'),
            ]
        } : {
            demo: [
                "babel-polyfill",
                resolve('./Thoth.Elmish.Demo.fsproj'),
                resolve('./src/scss/main.scss')
            ]
        },
    mode: isProduction ? "production" : "development",
    output: {
        path: resolve('./output'),
        filename: '[name].js'
    },
    plugins: commonPlugins,
    resolve: {
        modules: [
            "node_modules/",
            resolve("./node_modules")
        ]
    },
    devServer: {
        contentBase: resolve('./html/'),
        publicPath: "/",
        port: 8080
    },
    module: {
        rules: [
            {
                test: /\.fs(x|proj)?$/,
                use: {
                    loader: "fable-loader",
                    options: {
                        babel: babelOptions,
                        define: isProduction ? [] : ["DEBUG"],
                        extra: { optimizeWatch: true }
                    }
                }
            },
            {
                test: /\.js$/,
                exclude: /node_modules/,
                use: {
                    loader: 'babel-loader',
                    options: babelOptions
                },
            },
            {
                test: /\.s?[ac]ss$/,
                use: [
                    'css-loader',
                    'sass-loader',
                ],
            },
            {
                test: /\.css$/,
                use: ['style-loader', 'css-loader']
            },
            {
                test: /\.(png|jpg|jpeg|gif|svg|woff|woff2|ttf|eot)(\?.*$|$)/,
                use: ["file-loader"]
            }
        ]
    }
};
