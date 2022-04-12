const path = require("path");
const CopyPlugin = require("copy-webpack-plugin");
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin");

const dist = path.resolve(__dirname, "dist");

module.exports = {
  mode: "production",
  entry: {
    index: "./js/index.jsx",
  },

  // Something here fixes some dumb caching bug. Fucking stupid-ass, I
  // switched to this bullshit from Parcel and really, gotta appreciate the fucking
  // idiocy that causes this kind of nonsense to not only be necessary, but also
  // somehow considered "easy" and "useful". Fucking nonsense, jesus christ.
  // Hard enough to tell these days which part of "the stack" is broken, why the
  // ever living fuck is it this hard to watch the file system to changes and
  // rebuild? Why do we need "caching" systems that waste just as much dev time
  // breaking in silent and confusing ways as they save by not recompiling code
  // that should be trivial to recompile anyways. I fucking hate this shit.
  //
  //                                    - Albert Liu, Mar 27, 2022 Sun 22:34 EDT
  watchOptions: {
    aggregateTimeout: 200,
    poll: 200,
  },

  resolve: {
    alias: {
      react: "preact-compat",
      "react-dom": "preact-compat",
    },
  },

  output: {
    path: dist,
    filename: "[name].js",
  },

  devServer: {
    contentBase: dist,
    disableHostCheck: true,
  },

  module: {
    rules: [
      {
        test: /\.jsx?$/,
        exclude: /node_modules/,
        use: {
          loader: "babel-loader",
          options: {
            presets: ["@babel/preset-react", "@babel/preset-env"],
            plugins: [
              ["@babel/plugin-transform-runtime"],
              [
                "@babel/plugin-transform-react-jsx",
                {
                  pragma: "h",
                  pragmaFrag: "Fragment",
                },
              ],
            ],
          },
        },
      },
      {
        test: /\.(png|jpe?g|gif|woff|svg|eot|ttf)$/i,
        use: [{ loader: "file-loader" }],
      },
      {
        test: /\.css$/,
        use: [ "css-loader" ],
      },
    ],
  },

  plugins: [
    new CopyPlugin([path.resolve(__dirname, "static")]),

    new WasmPackPlugin({
      crateDirectory: __dirname,
    }),
  ],
};
