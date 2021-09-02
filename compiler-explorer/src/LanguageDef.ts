const languageDef = {
  keywords: [
    "true", "false", "ref", "inl", "inr",
    "fst", "snd", "case", "of", "if", "then",
    "else", "let", "fun", "in", "begin", "end",
    "while", "do", "()"
  ],

  typeKeywords: [
    'int', 'bool', 'unit', 'banter'
  ],

  operators: [
    ",", ":", ";", "+",
    "-", "*", "/", "~",
    "=", ":=", "<", "&&",
    "||", "|", "->", "?",
    "!", "()"
  ],

  // we include these common regular expressions
  symbols:  /[=><!~?:&|+\-*/^%]+/,

  // C# style strings
  escapes: /\\/,

  // The main tokenizer for our languages
  tokenizer: {
    root: [
      // identifiers and keywords
      [/[a-z_$][\w$]*/, { cases: { '@typeKeywords': 'keyword',
                                   '@keywords': 'keyword',
                                   '@default': 'identifier' } }],
      [/[A-Z][\w$]*/, 'type.identifier' ],  // to show class names nicely

      // whitespace
      { include: '@whitespace' },

      // delimiters and operators
      [/[{}()[\]]/, '@brackets'],
      [/[<>](?!@symbols)/, '@brackets'],
      [/@symbols/, { cases: { '@operators': 'operator',
                              '@default'  : '' } } ],

      // numbers
      [/\d*\.\d+([eE][-+]?\d+)?/, 'number.float'],
      [/0[xX][0-9a-fA-F]+/, 'number.hex'],
      [/\d+/, 'number'],

      // strings
      [/"([^"\\]|\\.)*$/, 'string.invalid' ],  // non-teminated string
      [/"/,  { token: 'string.quote', bracket: '@open', next: '@string' } ],

      // characters
      [/'[^\\']'/, 'string'],
      [/(')(@escapes)(')/, ['string','string.escape','string']],
      [/'/, 'string.invalid']
    ],

    comment: [
      [/[^(*]+/, 'comment' ],
      [/\(\*/,    'comment', '@push' ],    // nested comment
      ["\\*\\)",    'comment', '@pop'  ],
      [/[(*]/,   'comment' ]
    ],

    string: [
      [/[^\\"]+/,  'string'],
      [/@escapes/, 'string.escape'],
      [/\\./,      'string.escape.invalid'],
      [/"/,        { token: 'string.quote', bracket: '@close', next: '@pop' } ]
    ],

    whitespace: [
      [/[ \t\r\n]+/, 'white'],
      [/\(\*/,       'comment', '@comment' ],
    ],
  },
};

export default languageDef;
