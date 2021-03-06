const path = require("path");
const { merge } = require("webpack-merge");

const ClosurePlugin = require("closure-webpack-plugin");
const HTMLWebpackPlugin = require("html-webpack-plugin");
const { CleanWebpackPlugin } = require("clean-webpack-plugin");

// Production CSS assets - separate, minimised file
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const OptimizeCSSAssetsPlugin = require("optimize-css-assets-webpack-plugin");

var MODE = process.env.NODE_ENV === "production" ? "production" : "development";
var withDebug = !process.env["npm_config_nodebug"] && MODE === "development";
// this may help for Yarn users
// var withDebug = !npmParams.includes("--nodebug");
console.log(
  "\x1b[36m%s\x1b[0m",
  `** elm-webpack-starter: mode "${MODE}", withDebug: ${withDebug}\n`
);

var common = {
  mode: MODE,
  entry: "./src/index.js",
  output: {
    path: path.join(__dirname, "dist"),
    publicPath: "/",
    // FIXME webpack -p automatically adds hash when building for production
    filename: MODE === "production" ? "[name]-[hash].js" : "index.js",
  },
  plugins: [
    new HTMLWebpackPlugin({
      template: "src/index.html",
      inject: "body",
    }),
  ],
  resolve: {
    modules: [path.join(__dirname, "src"), "node_modules"],
    extensions: [".js", ".elm", ".scss", ".png"],
  },
  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: "babel-loader",
        },
      },
      {
        test: /\.css$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: [
          "style-loader",
          {
            loader: "css-loader",
            options: {
              importLoaders: 1,
            },
          },
          "postcss-loader",
        ],
      },
      {
        test: /\.(jpe?g|png|gif|svg)$/i,
        exclude: [/elm-stuff/, /node_modules/],
        loader: "file-loader",
      },
    ],
  },
};

if (MODE === "development") {
  module.exports = merge(common, {
    optimization: {
      noEmitOnErrors: true,
    },
    module: {
      rules: [
        {
          test: /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          use: [
            { loader: "elm-hot-webpack-loader" },
            {
              loader: "elm-webpack-loader",
              options: {
                debug: withDebug,
                forceWatch: true,
              },
            },
          ],
        },
      ],
    },
    devServer: {
      inline: true,
      stats: "errors-only",
      contentBase: path.join(__dirname, "src/assets"),
      historyApiFallback: true,
      // feel free to delete this section if you don't need anything like this
      before(app) {
        // on port 3000
        app.get("/test", function (req, res) {
          res.json({ result: "OK" });
        });
      },
    },
  });
}

if (MODE === "production") {
  module.exports = merge(common, {
    optimization: {
      minimizer: [
        new ClosurePlugin(
          { mode: "STANDARD" },
          {
            // compiler flags here
            //
            // for debugging help, try these:
            //
            // formatting: 'PRETTY_PRINT',
            // debug: true
            // renaming: false
          }
        ),
        new OptimizeCSSAssetsPlugin({}),
      ],
    },
    plugins: [
      // Delete everything from output-path (/dist) and report to user
      new CleanWebpackPlugin({
        root: __dirname,
        exclude: [],
        verbose: true,
        dry: false,
      }),
      new MiniCssExtractPlugin({
        // Options similar to the same options in webpackOptions.output
        // both options are optional
        filename: "[name]-[hash].css",
      }),
    ],
    module: {
      rules: [
        {
          test: /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          use: {
            loader: "elm-webpack-loader",
            options: {
              optimize: true,
            },
          },
        },
      ],
    },
  });
}
