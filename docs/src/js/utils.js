// <h2 id="primitives-decoders">
//     <a class="header-anchor" href="#primitives-decoders" aria-hidden="true">🔗</a> Primitives decoders</h2>

const md = require('markdown-it')({
        html: true
    })
    .use(require('markdown-it-highlightjs'))
    .use(require('${entryDir}/src/js/markdown-it-anchored'));

export function makeHtml(content) {
    return md.render(content);
}
