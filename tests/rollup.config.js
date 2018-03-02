import * as path from 'path';
import fable from 'rollup-plugin-fable';

function resolve(filePath) {
    return path.join(__dirname, filePath)
}

export default {
    input: resolve('./Thot.Tests.fsproj'),
    output: {
        file: resolve('./bin/tests.bundle.js'),
        format: 'cjs', // 'amd', 'cjs', 'es', 'iife', 'umd',
        external: ['xmlhttprequest'],
        name: 'FableREPL'
    },
    plugins: [
        fable({})
    ],
};
