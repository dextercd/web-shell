const { merge } = require('webpack-merge');
const common = require('./webpack.common.js');

module.exports = merge(common, {
    mode: 'production',
    output: {
        filename: '[name].[contenthash:6].js',
        assetModuleFilename: '[name].[contenthash:6][ext][query]',
    }
})
