const Prism = require('node-prismjs');
export function highlight(lang, sourceCode) {
    const language = Prism.languages[lang] || Prism.languages.autoit;
    return Prism.highlight(sourceCode, language);
}

const showdown = require('showdown');
showdown.extension('codehighlight', function () {
    function htmlunencode(text) {
        return (
            text
                .replace(/&amp;/g, '&')
                .replace(/&lt;/g, '<')
                .replace(/&gt;/g, '>')
        );
    }
    return [
        {
            type: 'output',
            filter: function (text, converter, options) {
                // use new shodown's regexp engine to conditionally parse codeblocks
                var left = '<pre><code\\b[^>]*>',
                    right = '</code></pre>',
                    flags = 'g',
                    replacement = function (wholeMatch, match, left, right) {
                        // unescape match to prevent double escaping
                        match = htmlunencode(match);
                        //  hljs.highlightAuto(match).value + right;
                        return left + highlight("fsharp", match) + right;
                    };
                return showdown.helper.replaceRecursiveRegExp(text, replacement, left, right, flags);
            }
        }
    ];
});

export function makeHtml(text) {
    const converter = new showdown.Converter({ extensions: ['codehighlight'] });
    return converter.makeHtml(text);
}
