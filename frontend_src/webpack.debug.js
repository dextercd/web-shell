const { merge } = require('webpack-merge');
const common = require('./webpack.common.js');

module.exports = merge(common, {
    mode: 'development',
    devtool: 'inline-source-map',
    output: {
        // Files with leading underscore shouldn't have long term caching
        filename: '_[name].js',
        assetModuleFilename: '_[name][ext][query]',
    }
})
