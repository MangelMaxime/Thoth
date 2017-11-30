const Prism = require('node-prismjs');
export function highlight(lang, sourceCode) {
  const language = Prism.languages[lang] || Prism.languages.autoit;
  return Prism.highlight(sourceCode, language);
}
