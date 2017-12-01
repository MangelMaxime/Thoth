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

showdown.extension('linked-title', function () {
    return [
        {
            type: 'output',
            filter: function (text, converter, options) {
                // use new shodown's regexp engine to conditionally parse codeblocks
                var left = '<h[123456]\\b[^>]*>',
                    right = '</h[123456]>',
                    flags = 'g',
                    replacement = function (wholeMatch, match, left, right) {
                        var tag =
                            // Borrowed from showdonws header.js
                            match.replace(/ /g, '-')
                            // replace previously escaped chars (&, ¨ and $)
                            .replace(/&amp;/g, '')
                            .replace(/¨T/g, '')
                            .replace(/¨D/g, '')
                            // replace rest of the chars (&~$ are repeated as they might have been escaped)
                            // borrowed from github's redcarpet (some they should produce similar results)
                            .replace(/[&+$,\/:;=?@"#{}|^¨~\[\]`\\*)(%.!'<>]/g, '')
                            .toLowerCase();

                        console.log(tag);

                        var anchor =
                            `<a href="#${tag}" class="anchor-control">
                                <span class="anchor" id="${tag}"></span>
                                <span class="icon">
                                    <i class="fa fa-lg fa-link"></i>
                                </span>
                            </a>`;
                        return left + anchor + match + right;
                    };
                return showdown.helper.replaceRecursiveRegExp(text, replacement, left, right, flags);
            }
        }
    ];
});

export function makeHtml(text) {
    const converter = new showdown.Converter({
        extensions: ['codehighlight', 'linked-title'],
        noHeaderId: true // Disable showdown headerId generation we use our custom generator to take acocunt of the fixed navbar
    });
    return converter.makeHtml(text);
}
