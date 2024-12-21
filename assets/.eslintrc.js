module.exports = {
  env: {
    browser: true,
    es2021: true
  },
  extends: 'standard',
  overrides: [
  ],
  parserOptions: {
    ecmaVersion: 'latest',
    sourceType: 'module'
  },
  rules: {
    'no-new': 0,
    'no-path-concat': 0,
    'no-throw-literal': 0,
    'no-useless-escape': 0,
    'object-curly-spacing': 0
  },
  globals: {
    'Handlebars': 'readonly'
  }
}
