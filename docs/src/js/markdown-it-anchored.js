const string = require('string')

const slugify = s =>
    string(s).slugify().toString()

const hasProp = ({}).hasOwnProperty

const permalinkHref = slug => `#${slug}`

const renderPermalink = (slug, state, idx) => {
    const space = () =>
        Object.assign(new state.Token('text', '', 0), { content: ' ' });

    const linkTokens = [
        Object.assign(new state.Token('link_open', 'a', 1), {
            attrs: [
                ['href', permalinkHref(slug, state)],
                ['aria-hidden', 'true']
            ]
        }),
        Object.assign(new state.Token('anchor_open', 'span', 1), {
            attrs: [
                ['class', 'anchor'],
                ['id', slug]
            ]
        }),
        new state.Token('anchor_open', 'span', -1),
        Object.assign(new state.Token('html_block', '', 0), { content: '#' }),
        new state.Token('link_close', 'a', -1)
    ];

    // `push` or `unshift` according to position option.
    // Space is at the opposite side.
    linkTokens.push(space());
    state.tokens[idx + 1].children.unshift(...linkTokens);
}

const uniqueSlug = (slug, slugs) => {
    // Mark this slug as used in the environment.
    slugs[slug] = (hasProp.call(slugs, slug) ? slugs[slug] : 0) + 1

    // First slug, return as is.
    if (slugs[slug] === 1) {
        return slug
    }

    // Duplicate slug, add a `-2`, `-3`, etc. to keep ID unique.
    return slug + '-' + slugs[slug]
}

const isLevelSelectedNumber = selection => level => level >= selection
const isLevelSelectedArray = selection => level => selection.includes(level)

const anchor = (md) => {

    md.core.ruler.push('anchor', state => {
        const slugs = {}
        const tokens = state.tokens

        tokens
            .filter(token => token.type === 'heading_open')
            .forEach(token => {
                // Aggregate the next token children text.
                const title = tokens[tokens.indexOf(token) + 1].children
                    .filter(token => token.type === 'text' || token.type === 'code_inline')
                    .reduce((acc, t) => acc + t.content, '');

                const slug = uniqueSlug(slugify(title), slugs);

                renderPermalink(slug, state, tokens.indexOf(token));
            });
    })
}

module.exports = anchor;
