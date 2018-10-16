const path = require("path");
const webpack = require("webpack");

function resolve(filePath) {
    return path.join(__dirname, filePath)
}

var babelOptions = {
    presets: [
        ["@babel/preset-env", {
            "targets": {
                "browsers": ["last 2 versions"]
            },
            "modules": false
        }],
        "@babel/react"
    ],
    plugins: [
        "@babel/plugin-proposal-class-properties"
    ]
};

var isProduction = !process.argv.find(v => v.indexOf('webpack-dev-server') !== -1);
console.log("Bundling for " + (isProduction ? "production" : "development") + "...");

var commonPlugins = [
];

module.exports = {
    devtool: undefined,
    entry: isProduction ? // We don't use the same entry for dev and production, to make HMR over style quicker for dev env
        {
            // We don't output style because it's already included by the docs
            demo: [
                "@babel/polyfill",
                './Thoth.Elmish.Demo.fsproj'
            ]
        } : {
            demo: [
                "@babel/polyfill",
                './Thoth.Elmish.Demo.fsproj',
                './src/scss/main.scss'
            ]
        },
    mode: isProduction ? "production" : "development",
    optimization: {
        splitChunks: {
            cacheGroups: {
                commons: {
                    test: /node_modules/,
                    name: "vendors",
                    chunks: "all"
                }
            }
        },
    },
    output: {
        path: resolve('./output'),
        filename: '[name].js'
    },
    plugins: isProduction ?
        commonPlugins
        : commonPlugins.concat([
            new webpack.HotModuleReplacementPlugin(),
            new webpack.NamedModulesPlugin()
        ]),
    devServer: {
        contentBase: './html/',
        publicPath: "/",
        port: 8080,
        hot: true,
        inline: true
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
                test: /\.(sass|scss)$/,
                use: [
                    'style-loader',
                    'css-loader',
                    'sass-loader',
                ],
            },
            {
                test: /\.css$/,
                use: ['style-loader', 'css-loader']
            },
            {
                test: /\.(png|jpg|jpeg|gif|svg|woff|woff2|ttf|eot)$/,
                use: ["file-loader"]
            }
        ]
    }
};
