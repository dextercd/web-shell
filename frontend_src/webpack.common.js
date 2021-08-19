const { WebpackManifestPlugin } = require('webpack-manifest-plugin');

module.exports = {
  mode: 'development',

  entry: './src/index.ts',

  output: {
    path: __dirname + '/../priv/static/dist',
    clean: true,
    publicPath: '/static/dist/',
  },

  cache: {
    type: 'filesystem',
  },

  optimization: {
    // Makes it so the contenthash is actually stable
    runtimeChunk: 'single',
    moduleIds: 'deterministic',
  },

  resolve: {
    extensions: ['.ts', '.tsx', '.js'],
  },

  module: {
    rules: [
      {test: /\.tsx?$/, loader: 'ts-loader'},
      {test: /src\/generated\//, type: 'asset/resource'}
    ]
  },

  plugins: [
    new WebpackManifestPlugin(),
  ],
}
