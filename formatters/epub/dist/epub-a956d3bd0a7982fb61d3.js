/******/ (function(modules) { // webpackBootstrap
/******/ 	// The module cache
/******/ 	var installedModules = {};
/******/
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/
/******/ 		// Check if module is in cache
/******/ 		if(installedModules[moduleId]) {
/******/ 			return installedModules[moduleId].exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = installedModules[moduleId] = {
/******/ 			i: moduleId,
/******/ 			l: false,
/******/ 			exports: {}
/******/ 		};
/******/
/******/ 		// Execute the module function
/******/ 		modules[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/
/******/ 		// Flag the module as loaded
/******/ 		module.l = true;
/******/
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/
/******/
/******/ 	// expose the modules object (__webpack_modules__)
/******/ 	__webpack_require__.m = modules;
/******/
/******/ 	// expose the module cache
/******/ 	__webpack_require__.c = installedModules;
/******/
/******/ 	// define getter function for harmony exports
/******/ 	__webpack_require__.d = function(exports, name, getter) {
/******/ 		if(!__webpack_require__.o(exports, name)) {
/******/ 			Object.defineProperty(exports, name, { enumerable: true, get: getter });
/******/ 		}
/******/ 	};
/******/
/******/ 	// define __esModule on exports
/******/ 	__webpack_require__.r = function(exports) {
/******/ 		if(typeof Symbol !== 'undefined' && Symbol.toStringTag) {
/******/ 			Object.defineProperty(exports, Symbol.toStringTag, { value: 'Module' });
/******/ 		}
/******/ 		Object.defineProperty(exports, '__esModule', { value: true });
/******/ 	};
/******/
/******/ 	// create a fake namespace object
/******/ 	// mode & 1: value is a module id, require it
/******/ 	// mode & 2: merge all properties of value into the ns
/******/ 	// mode & 4: return value when already ns object
/******/ 	// mode & 8|1: behave like require
/******/ 	__webpack_require__.t = function(value, mode) {
/******/ 		if(mode & 1) value = __webpack_require__(value);
/******/ 		if(mode & 8) return value;
/******/ 		if((mode & 4) && typeof value === 'object' && value && value.__esModule) return value;
/******/ 		var ns = Object.create(null);
/******/ 		__webpack_require__.r(ns);
/******/ 		Object.defineProperty(ns, 'default', { enumerable: true, value: value });
/******/ 		if(mode & 2 && typeof value != 'string') for(var key in value) __webpack_require__.d(ns, key, function(key) { return value[key]; }.bind(null, key));
/******/ 		return ns;
/******/ 	};
/******/
/******/ 	// getDefaultExport function for compatibility with non-harmony modules
/******/ 	__webpack_require__.n = function(module) {
/******/ 		var getter = module && module.__esModule ?
/******/ 			function getDefault() { return module['default']; } :
/******/ 			function getModuleExports() { return module; };
/******/ 		__webpack_require__.d(getter, 'a', getter);
/******/ 		return getter;
/******/ 	};
/******/
/******/ 	// Object.prototype.hasOwnProperty.call
/******/ 	__webpack_require__.o = function(object, property) { return Object.prototype.hasOwnProperty.call(object, property); };
/******/
/******/ 	// __webpack_public_path__
/******/ 	__webpack_require__.p = "";
/******/
/******/
/******/ 	// Load entry module and return exports
/******/ 	return __webpack_require__(__webpack_require__.s = 35);
/******/ })
/************************************************************************/
/******/ ([
/* 0 */,
/* 1 */
/***/ (function(module, exports, __webpack_require__) {

/*
Syntax highlighting with language autodetection.
https://highlightjs.org/
*/

(function(factory) {

  // Find the global object for export to both the browser and web workers.
  var globalObject = typeof window === 'object' && window ||
                     typeof self === 'object' && self;

  // Setup highlight.js for different environments. First is Node.js or
  // CommonJS.
  if(true) {
    factory(exports);
  } else {}

}(function(hljs) {
  // Convenience variables for build-in objects
  var ArrayProto = [],
      objectKeys = Object.keys;

  // Global internal variables used within the highlight.js library.
  var languages = {},
      aliases   = {};

  // Regular expressions used throughout the highlight.js library.
  var noHighlightRe    = /^(no-?highlight|plain|text)$/i,
      languagePrefixRe = /\blang(?:uage)?-([\w-]+)\b/i,
      fixMarkupRe      = /((^(<[^>]+>|\t|)+|(?:\n)))/gm;

  var spanEndTag = '</span>';

  // Global options used when within external APIs. This is modified when
  // calling the `hljs.configure` function.
  var options = {
    classPrefix: 'hljs-',
    tabReplace: null,
    useBR: false,
    languages: undefined
  };


  /* Utility functions */

  function escape(value) {
    return value.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  }

  function tag(node) {
    return node.nodeName.toLowerCase();
  }

  function testRe(re, lexeme) {
    var match = re && re.exec(lexeme);
    return match && match.index === 0;
  }

  function isNotHighlighted(language) {
    return noHighlightRe.test(language);
  }

  function blockLanguage(block) {
    var i, match, length, _class;
    var classes = block.className + ' ';

    classes += block.parentNode ? block.parentNode.className : '';

    // language-* takes precedence over non-prefixed class names.
    match = languagePrefixRe.exec(classes);
    if (match) {
      return getLanguage(match[1]) ? match[1] : 'no-highlight';
    }

    classes = classes.split(/\s+/);

    for (i = 0, length = classes.length; i < length; i++) {
      _class = classes[i]

      if (isNotHighlighted(_class) || getLanguage(_class)) {
        return _class;
      }
    }
  }

  function inherit(parent) {  // inherit(parent, override_obj, override_obj, ...)
    var key;
    var result = {};
    var objects = Array.prototype.slice.call(arguments, 1);

    for (key in parent)
      result[key] = parent[key];
    objects.forEach(function(obj) {
      for (key in obj)
        result[key] = obj[key];
    });
    return result;
  }

  /* Stream merging */

  function nodeStream(node) {
    var result = [];
    (function _nodeStream(node, offset) {
      for (var child = node.firstChild; child; child = child.nextSibling) {
        if (child.nodeType === 3)
          offset += child.nodeValue.length;
        else if (child.nodeType === 1) {
          result.push({
            event: 'start',
            offset: offset,
            node: child
          });
          offset = _nodeStream(child, offset);
          // Prevent void elements from having an end tag that would actually
          // double them in the output. There are more void elements in HTML
          // but we list only those realistically expected in code display.
          if (!tag(child).match(/br|hr|img|input/)) {
            result.push({
              event: 'stop',
              offset: offset,
              node: child
            });
          }
        }
      }
      return offset;
    })(node, 0);
    return result;
  }

  function mergeStreams(original, highlighted, value) {
    var processed = 0;
    var result = '';
    var nodeStack = [];

    function selectStream() {
      if (!original.length || !highlighted.length) {
        return original.length ? original : highlighted;
      }
      if (original[0].offset !== highlighted[0].offset) {
        return (original[0].offset < highlighted[0].offset) ? original : highlighted;
      }

      /*
      To avoid starting the stream just before it should stop the order is
      ensured that original always starts first and closes last:

      if (event1 == 'start' && event2 == 'start')
        return original;
      if (event1 == 'start' && event2 == 'stop')
        return highlighted;
      if (event1 == 'stop' && event2 == 'start')
        return original;
      if (event1 == 'stop' && event2 == 'stop')
        return highlighted;

      ... which is collapsed to:
      */
      return highlighted[0].event === 'start' ? original : highlighted;
    }

    function open(node) {
      function attr_str(a) {return ' ' + a.nodeName + '="' + escape(a.value).replace('"', '&quot;') + '"';}
      result += '<' + tag(node) + ArrayProto.map.call(node.attributes, attr_str).join('') + '>';
    }

    function close(node) {
      result += '</' + tag(node) + '>';
    }

    function render(event) {
      (event.event === 'start' ? open : close)(event.node);
    }

    while (original.length || highlighted.length) {
      var stream = selectStream();
      result += escape(value.substring(processed, stream[0].offset));
      processed = stream[0].offset;
      if (stream === original) {
        /*
        On any opening or closing tag of the original markup we first close
        the entire highlighted node stack, then render the original tag along
        with all the following original tags at the same offset and then
        reopen all the tags on the highlighted stack.
        */
        nodeStack.reverse().forEach(close);
        do {
          render(stream.splice(0, 1)[0]);
          stream = selectStream();
        } while (stream === original && stream.length && stream[0].offset === processed);
        nodeStack.reverse().forEach(open);
      } else {
        if (stream[0].event === 'start') {
          nodeStack.push(stream[0].node);
        } else {
          nodeStack.pop();
        }
        render(stream.splice(0, 1)[0]);
      }
    }
    return result + escape(value.substr(processed));
  }

  /* Initialization */

  function expand_mode(mode) {
    if (mode.variants && !mode.cached_variants) {
      mode.cached_variants = mode.variants.map(function(variant) {
        return inherit(mode, {variants: null}, variant);
      });
    }
    return mode.cached_variants || (mode.endsWithParent && [inherit(mode)]) || [mode];
  }

  function compileLanguage(language) {

    function reStr(re) {
        return (re && re.source) || re;
    }

    function langRe(value, global) {
      return new RegExp(
        reStr(value),
        'm' + (language.case_insensitive ? 'i' : '') + (global ? 'g' : '')
      );
    }

    function compileMode(mode, parent) {
      if (mode.compiled)
        return;
      mode.compiled = true;

      mode.keywords = mode.keywords || mode.beginKeywords;
      if (mode.keywords) {
        var compiled_keywords = {};

        var flatten = function(className, str) {
          if (language.case_insensitive) {
            str = str.toLowerCase();
          }
          str.split(' ').forEach(function(kw) {
            var pair = kw.split('|');
            compiled_keywords[pair[0]] = [className, pair[1] ? Number(pair[1]) : 1];
          });
        };

        if (typeof mode.keywords === 'string') { // string
          flatten('keyword', mode.keywords);
        } else {
          objectKeys(mode.keywords).forEach(function (className) {
            flatten(className, mode.keywords[className]);
          });
        }
        mode.keywords = compiled_keywords;
      }
      mode.lexemesRe = langRe(mode.lexemes || /\w+/, true);

      if (parent) {
        if (mode.beginKeywords) {
          mode.begin = '\\b(' + mode.beginKeywords.split(' ').join('|') + ')\\b';
        }
        if (!mode.begin)
          mode.begin = /\B|\b/;
        mode.beginRe = langRe(mode.begin);
        if (!mode.end && !mode.endsWithParent)
          mode.end = /\B|\b/;
        if (mode.end)
          mode.endRe = langRe(mode.end);
        mode.terminator_end = reStr(mode.end) || '';
        if (mode.endsWithParent && parent.terminator_end)
          mode.terminator_end += (mode.end ? '|' : '') + parent.terminator_end;
      }
      if (mode.illegal)
        mode.illegalRe = langRe(mode.illegal);
      if (mode.relevance == null)
        mode.relevance = 1;
      if (!mode.contains) {
        mode.contains = [];
      }
      mode.contains = Array.prototype.concat.apply([], mode.contains.map(function(c) {
        return expand_mode(c === 'self' ? mode : c)
      }));
      mode.contains.forEach(function(c) {compileMode(c, mode);});

      if (mode.starts) {
        compileMode(mode.starts, parent);
      }

      var terminators =
        mode.contains.map(function(c) {
          return c.beginKeywords ? '\\.?(' + c.begin + ')\\.?' : c.begin;
        })
        .concat([mode.terminator_end, mode.illegal])
        .map(reStr)
        .filter(Boolean);
      mode.terminators = terminators.length ? langRe(terminators.join('|'), true) : {exec: function(/*s*/) {return null;}};
    }

    compileMode(language);
  }

  /*
  Core highlighting function. Accepts a language name, or an alias, and a
  string with the code to highlight. Returns an object with the following
  properties:

  - relevance (int)
  - value (an HTML string with highlighting markup)

  */
  function highlight(name, value, ignore_illegals, continuation) {

    function subMode(lexeme, mode) {
      var i, length;

      for (i = 0, length = mode.contains.length; i < length; i++) {
        if (testRe(mode.contains[i].beginRe, lexeme)) {
          return mode.contains[i];
        }
      }
    }

    function endOfMode(mode, lexeme) {
      if (testRe(mode.endRe, lexeme)) {
        while (mode.endsParent && mode.parent) {
          mode = mode.parent;
        }
        return mode;
      }
      if (mode.endsWithParent) {
        return endOfMode(mode.parent, lexeme);
      }
    }

    function isIllegal(lexeme, mode) {
      return !ignore_illegals && testRe(mode.illegalRe, lexeme);
    }

    function keywordMatch(mode, match) {
      var match_str = language.case_insensitive ? match[0].toLowerCase() : match[0];
      return mode.keywords.hasOwnProperty(match_str) && mode.keywords[match_str];
    }

    function buildSpan(classname, insideSpan, leaveOpen, noPrefix) {
      var classPrefix = noPrefix ? '' : options.classPrefix,
          openSpan    = '<span class="' + classPrefix,
          closeSpan   = leaveOpen ? '' : spanEndTag

      openSpan += classname + '">';

      return openSpan + insideSpan + closeSpan;
    }

    function processKeywords() {
      var keyword_match, last_index, match, result;

      if (!top.keywords)
        return escape(mode_buffer);

      result = '';
      last_index = 0;
      top.lexemesRe.lastIndex = 0;
      match = top.lexemesRe.exec(mode_buffer);

      while (match) {
        result += escape(mode_buffer.substring(last_index, match.index));
        keyword_match = keywordMatch(top, match);
        if (keyword_match) {
          relevance += keyword_match[1];
          result += buildSpan(keyword_match[0], escape(match[0]));
        } else {
          result += escape(match[0]);
        }
        last_index = top.lexemesRe.lastIndex;
        match = top.lexemesRe.exec(mode_buffer);
      }
      return result + escape(mode_buffer.substr(last_index));
    }

    function processSubLanguage() {
      var explicit = typeof top.subLanguage === 'string';
      if (explicit && !languages[top.subLanguage]) {
        return escape(mode_buffer);
      }

      var result = explicit ?
                   highlight(top.subLanguage, mode_buffer, true, continuations[top.subLanguage]) :
                   highlightAuto(mode_buffer, top.subLanguage.length ? top.subLanguage : undefined);

      // Counting embedded language score towards the host language may be disabled
      // with zeroing the containing mode relevance. Usecase in point is Markdown that
      // allows XML everywhere and makes every XML snippet to have a much larger Markdown
      // score.
      if (top.relevance > 0) {
        relevance += result.relevance;
      }
      if (explicit) {
        continuations[top.subLanguage] = result.top;
      }
      return buildSpan(result.language, result.value, false, true);
    }

    function processBuffer() {
      result += (top.subLanguage != null ? processSubLanguage() : processKeywords());
      mode_buffer = '';
    }

    function startNewMode(mode) {
      result += mode.className? buildSpan(mode.className, '', true): '';
      top = Object.create(mode, {parent: {value: top}});
    }

    function processLexeme(buffer, lexeme) {

      mode_buffer += buffer;

      if (lexeme == null) {
        processBuffer();
        return 0;
      }

      var new_mode = subMode(lexeme, top);
      if (new_mode) {
        if (new_mode.skip) {
          mode_buffer += lexeme;
        } else {
          if (new_mode.excludeBegin) {
            mode_buffer += lexeme;
          }
          processBuffer();
          if (!new_mode.returnBegin && !new_mode.excludeBegin) {
            mode_buffer = lexeme;
          }
        }
        startNewMode(new_mode, lexeme);
        return new_mode.returnBegin ? 0 : lexeme.length;
      }

      var end_mode = endOfMode(top, lexeme);
      if (end_mode) {
        var origin = top;
        if (origin.skip) {
          mode_buffer += lexeme;
        } else {
          if (!(origin.returnEnd || origin.excludeEnd)) {
            mode_buffer += lexeme;
          }
          processBuffer();
          if (origin.excludeEnd) {
            mode_buffer = lexeme;
          }
        }
        do {
          if (top.className) {
            result += spanEndTag;
          }
          if (!top.skip) {
            relevance += top.relevance;
          }
          top = top.parent;
        } while (top !== end_mode.parent);
        if (end_mode.starts) {
          startNewMode(end_mode.starts, '');
        }
        return origin.returnEnd ? 0 : lexeme.length;
      }

      if (isIllegal(lexeme, top))
        throw new Error('Illegal lexeme "' + lexeme + '" for mode "' + (top.className || '<unnamed>') + '"');

      /*
      Parser should not reach this point as all types of lexemes should be caught
      earlier, but if it does due to some bug make sure it advances at least one
      character forward to prevent infinite looping.
      */
      mode_buffer += lexeme;
      return lexeme.length || 1;
    }

    var language = getLanguage(name);
    if (!language) {
      throw new Error('Unknown language: "' + name + '"');
    }

    compileLanguage(language);
    var top = continuation || language;
    var continuations = {}; // keep continuations for sub-languages
    var result = '', current;
    for(current = top; current !== language; current = current.parent) {
      if (current.className) {
        result = buildSpan(current.className, '', true) + result;
      }
    }
    var mode_buffer = '';
    var relevance = 0;
    try {
      var match, count, index = 0;
      while (true) {
        top.terminators.lastIndex = index;
        match = top.terminators.exec(value);
        if (!match)
          break;
        count = processLexeme(value.substring(index, match.index), match[0]);
        index = match.index + count;
      }
      processLexeme(value.substr(index));
      for(current = top; current.parent; current = current.parent) { // close dangling modes
        if (current.className) {
          result += spanEndTag;
        }
      }
      return {
        relevance: relevance,
        value: result,
        language: name,
        top: top
      };
    } catch (e) {
      if (e.message && e.message.indexOf('Illegal') !== -1) {
        return {
          relevance: 0,
          value: escape(value)
        };
      } else {
        throw e;
      }
    }
  }

  /*
  Highlighting with language detection. Accepts a string with the code to
  highlight. Returns an object with the following properties:

  - language (detected language)
  - relevance (int)
  - value (an HTML string with highlighting markup)
  - second_best (object with the same structure for second-best heuristically
    detected language, may be absent)

  */
  function highlightAuto(text, languageSubset) {
    languageSubset = languageSubset || options.languages || objectKeys(languages);
    var result = {
      relevance: 0,
      value: escape(text)
    };
    var second_best = result;
    languageSubset.filter(getLanguage).forEach(function(name) {
      var current = highlight(name, text, false);
      current.language = name;
      if (current.relevance > second_best.relevance) {
        second_best = current;
      }
      if (current.relevance > result.relevance) {
        second_best = result;
        result = current;
      }
    });
    if (second_best.language) {
      result.second_best = second_best;
    }
    return result;
  }

  /*
  Post-processing of the highlighted markup:

  - replace TABs with something more useful
  - replace real line-breaks with '<br>' for non-pre containers

  */
  function fixMarkup(value) {
    return !(options.tabReplace || options.useBR)
      ? value
      : value.replace(fixMarkupRe, function(match, p1) {
          if (options.useBR && match === '\n') {
            return '<br>';
          } else if (options.tabReplace) {
            return p1.replace(/\t/g, options.tabReplace);
          }
          return '';
      });
  }

  function buildClassName(prevClassName, currentLang, resultLang) {
    var language = currentLang ? aliases[currentLang] : resultLang,
        result   = [prevClassName.trim()];

    if (!prevClassName.match(/\bhljs\b/)) {
      result.push('hljs');
    }

    if (prevClassName.indexOf(language) === -1) {
      result.push(language);
    }

    return result.join(' ').trim();
  }

  /*
  Applies highlighting to a DOM node containing code. Accepts a DOM node and
  two optional parameters for fixMarkup.
  */
  function highlightBlock(block) {
    var node, originalStream, result, resultNode, text;
    var language = blockLanguage(block);

    if (isNotHighlighted(language))
        return;

    if (options.useBR) {
      node = document.createElementNS('http://www.w3.org/1999/xhtml', 'div');
      node.innerHTML = block.innerHTML.replace(/\n/g, '').replace(/<br[ \/]*>/g, '\n');
    } else {
      node = block;
    }
    text = node.textContent;
    result = language ? highlight(language, text, true) : highlightAuto(text);

    originalStream = nodeStream(node);
    if (originalStream.length) {
      resultNode = document.createElementNS('http://www.w3.org/1999/xhtml', 'div');
      resultNode.innerHTML = result.value;
      result.value = mergeStreams(originalStream, nodeStream(resultNode), text);
    }
    result.value = fixMarkup(result.value);

    block.innerHTML = result.value;
    block.className = buildClassName(block.className, language, result.language);
    block.result = {
      language: result.language,
      re: result.relevance
    };
    if (result.second_best) {
      block.second_best = {
        language: result.second_best.language,
        re: result.second_best.relevance
      };
    }
  }

  /*
  Updates highlight.js global options with values passed in the form of an object.
  */
  function configure(user_options) {
    options = inherit(options, user_options);
  }

  /*
  Applies highlighting to all <pre><code>..</code></pre> blocks on a page.
  */
  function initHighlighting() {
    if (initHighlighting.called)
      return;
    initHighlighting.called = true;

    var blocks = document.querySelectorAll('pre code');
    ArrayProto.forEach.call(blocks, highlightBlock);
  }

  /*
  Attaches highlighting to the page load event.
  */
  function initHighlightingOnLoad() {
    addEventListener('DOMContentLoaded', initHighlighting, false);
    addEventListener('load', initHighlighting, false);
  }

  function registerLanguage(name, language) {
    var lang = languages[name] = language(hljs);
    if (lang.aliases) {
      lang.aliases.forEach(function(alias) {aliases[alias] = name;});
    }
  }

  function listLanguages() {
    return objectKeys(languages);
  }

  function getLanguage(name) {
    name = (name || '').toLowerCase();
    return languages[name] || languages[aliases[name]];
  }

  /* Interface definition */

  hljs.highlight = highlight;
  hljs.highlightAuto = highlightAuto;
  hljs.fixMarkup = fixMarkup;
  hljs.highlightBlock = highlightBlock;
  hljs.configure = configure;
  hljs.initHighlighting = initHighlighting;
  hljs.initHighlightingOnLoad = initHighlightingOnLoad;
  hljs.registerLanguage = registerLanguage;
  hljs.listLanguages = listLanguages;
  hljs.getLanguage = getLanguage;
  hljs.inherit = inherit;

  // Common regexps
  hljs.IDENT_RE = '[a-zA-Z]\\w*';
  hljs.UNDERSCORE_IDENT_RE = '[a-zA-Z_]\\w*';
  hljs.NUMBER_RE = '\\b\\d+(\\.\\d+)?';
  hljs.C_NUMBER_RE = '(-?)(\\b0[xX][a-fA-F0-9]+|(\\b\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?)'; // 0x..., 0..., decimal, float
  hljs.BINARY_NUMBER_RE = '\\b(0b[01]+)'; // 0b...
  hljs.RE_STARTERS_RE = '!|!=|!==|%|%=|&|&&|&=|\\*|\\*=|\\+|\\+=|,|-|-=|/=|/|:|;|<<|<<=|<=|<|===|==|=|>>>=|>>=|>=|>>>|>>|>|\\?|\\[|\\{|\\(|\\^|\\^=|\\||\\|=|\\|\\||~';

  // Common modes
  hljs.BACKSLASH_ESCAPE = {
    begin: '\\\\[\\s\\S]', relevance: 0
  };
  hljs.APOS_STRING_MODE = {
    className: 'string',
    begin: '\'', end: '\'',
    illegal: '\\n',
    contains: [hljs.BACKSLASH_ESCAPE]
  };
  hljs.QUOTE_STRING_MODE = {
    className: 'string',
    begin: '"', end: '"',
    illegal: '\\n',
    contains: [hljs.BACKSLASH_ESCAPE]
  };
  hljs.PHRASAL_WORDS_MODE = {
    begin: /\b(a|an|the|are|I'm|isn't|don't|doesn't|won't|but|just|should|pretty|simply|enough|gonna|going|wtf|so|such|will|you|your|they|like|more)\b/
  };
  hljs.COMMENT = function (begin, end, inherits) {
    var mode = hljs.inherit(
      {
        className: 'comment',
        begin: begin, end: end,
        contains: []
      },
      inherits || {}
    );
    mode.contains.push(hljs.PHRASAL_WORDS_MODE);
    mode.contains.push({
      className: 'doctag',
      begin: '(?:TODO|FIXME|NOTE|BUG|XXX):',
      relevance: 0
    });
    return mode;
  };
  hljs.C_LINE_COMMENT_MODE = hljs.COMMENT('//', '$');
  hljs.C_BLOCK_COMMENT_MODE = hljs.COMMENT('/\\*', '\\*/');
  hljs.HASH_COMMENT_MODE = hljs.COMMENT('#', '$');
  hljs.NUMBER_MODE = {
    className: 'number',
    begin: hljs.NUMBER_RE,
    relevance: 0
  };
  hljs.C_NUMBER_MODE = {
    className: 'number',
    begin: hljs.C_NUMBER_RE,
    relevance: 0
  };
  hljs.BINARY_NUMBER_MODE = {
    className: 'number',
    begin: hljs.BINARY_NUMBER_RE,
    relevance: 0
  };
  hljs.CSS_NUMBER_MODE = {
    className: 'number',
    begin: hljs.NUMBER_RE + '(' +
      '%|em|ex|ch|rem'  +
      '|vw|vh|vmin|vmax' +
      '|cm|mm|in|pt|pc|px' +
      '|deg|grad|rad|turn' +
      '|s|ms' +
      '|Hz|kHz' +
      '|dpi|dpcm|dppx' +
      ')?',
    relevance: 0
  };
  hljs.REGEXP_MODE = {
    className: 'regexp',
    begin: /\//, end: /\/[gimuy]*/,
    illegal: /\n/,
    contains: [
      hljs.BACKSLASH_ESCAPE,
      {
        begin: /\[/, end: /\]/,
        relevance: 0,
        contains: [hljs.BACKSLASH_ESCAPE]
      }
    ]
  };
  hljs.TITLE_MODE = {
    className: 'title',
    begin: hljs.IDENT_RE,
    relevance: 0
  };
  hljs.UNDERSCORE_TITLE_MODE = {
    className: 'title',
    begin: hljs.UNDERSCORE_IDENT_RE,
    relevance: 0
  };
  hljs.METHOD_GUARD = {
    // excludes method names from keyword processing
    begin: '\\.\\s*' + hljs.UNDERSCORE_IDENT_RE,
    relevance: 0
  };

  return hljs;
}));


/***/ }),
/* 2 */,
/* 3 */,
/* 4 */,
/* 5 */,
/* 6 */
/***/ (function(module, exports) {

module.exports = function(hljs) {
  var VAR = {
    className: 'variable',
    variants: [
      {begin: /\$[\w\d#@][\w\d_]*/},
      {begin: /\$\{(.*?)}/}
    ]
  };
  var QUOTE_STRING = {
    className: 'string',
    begin: /"/, end: /"/,
    contains: [
      hljs.BACKSLASH_ESCAPE,
      VAR,
      {
        className: 'variable',
        begin: /\$\(/, end: /\)/,
        contains: [hljs.BACKSLASH_ESCAPE]
      }
    ]
  };
  var APOS_STRING = {
    className: 'string',
    begin: /'/, end: /'/
  };

  return {
    aliases: ['sh', 'zsh'],
    lexemes: /-?[a-z\._]+/,
    keywords: {
      keyword:
        'if then else elif fi for while in do done case esac function',
      literal:
        'true false',
      built_in:
        // Shell built-ins
        // http://www.gnu.org/software/bash/manual/html_node/Shell-Builtin-Commands.html
        'break cd continue eval exec exit export getopts hash pwd readonly return shift test times ' +
        'trap umask unset ' +
        // Bash built-ins
        'alias bind builtin caller command declare echo enable help let local logout mapfile printf ' +
        'read readarray source type typeset ulimit unalias ' +
        // Shell modifiers
        'set shopt ' +
        // Zsh built-ins
        'autoload bg bindkey bye cap chdir clone comparguments compcall compctl compdescribe compfiles ' +
        'compgroups compquote comptags comptry compvalues dirs disable disown echotc echoti emulate ' +
        'fc fg float functions getcap getln history integer jobs kill limit log noglob popd print ' +
        'pushd pushln rehash sched setcap setopt stat suspend ttyctl unfunction unhash unlimit ' +
        'unsetopt vared wait whence where which zcompile zformat zftp zle zmodload zparseopts zprof ' +
        'zpty zregexparse zsocket zstyle ztcp',
      _:
        '-ne -eq -lt -gt -f -d -e -s -l -a' // relevance booster
    },
    contains: [
      {
        className: 'meta',
        begin: /^#![^\n]+sh\s*$/,
        relevance: 10
      },
      {
        className: 'function',
        begin: /\w[\w\d_]*\s*\(\s*\)\s*\{/,
        returnBegin: true,
        contains: [hljs.inherit(hljs.TITLE_MODE, {begin: /\w[\w\d_]*/})],
        relevance: 0
      },
      hljs.HASH_COMMENT_MODE,
      QUOTE_STRING,
      APOS_STRING,
      VAR
    ]
  };
};

/***/ }),
/* 7 */
/***/ (function(module, exports) {

module.exports = function(hljs) {
  var IDENT_RE = '[a-zA-Z-][a-zA-Z0-9_-]*';
  var RULE = {
    begin: /[A-Z\_\.\-]+\s*:/, returnBegin: true, end: ';', endsWithParent: true,
    contains: [
      {
        className: 'attribute',
        begin: /\S/, end: ':', excludeEnd: true,
        starts: {
          endsWithParent: true, excludeEnd: true,
          contains: [
            {
              begin: /[\w-]+\(/, returnBegin: true,
              contains: [
                {
                  className: 'built_in',
                  begin: /[\w-]+/
                },
                {
                  begin: /\(/, end: /\)/,
                  contains: [
                    hljs.APOS_STRING_MODE,
                    hljs.QUOTE_STRING_MODE
                  ]
                }
              ]
            },
            hljs.CSS_NUMBER_MODE,
            hljs.QUOTE_STRING_MODE,
            hljs.APOS_STRING_MODE,
            hljs.C_BLOCK_COMMENT_MODE,
            {
              className: 'number', begin: '#[0-9A-Fa-f]+'
            },
            {
              className: 'meta', begin: '!important'
            }
          ]
        }
      }
    ]
  };

  return {
    case_insensitive: true,
    illegal: /[=\/|'\$]/,
    contains: [
      hljs.C_BLOCK_COMMENT_MODE,
      {
        className: 'selector-id', begin: /#[A-Za-z0-9_-]+/
      },
      {
        className: 'selector-class', begin: /\.[A-Za-z0-9_-]+/
      },
      {
        className: 'selector-attr',
        begin: /\[/, end: /\]/,
        illegal: '$'
      },
      {
        className: 'selector-pseudo',
        begin: /:(:)?[a-zA-Z0-9\_\-\+\(\)"'.]+/
      },
      {
        begin: '@(font-face|page)',
        lexemes: '[a-z-]+',
        keywords: 'font-face page'
      },
      {
        begin: '@', end: '[{;]', // at_rule eating first "{" is a good thing
                                 // because it doesn’t let it to be parsed as
                                 // a rule set but instead drops parser into
                                 // the default mode which is how it should be.
        illegal: /:/, // break on Less variables @var: ...
        contains: [
          {
            className: 'keyword',
            begin: /\w+/
          },
          {
            begin: /\s/, endsWithParent: true, excludeEnd: true,
            relevance: 0,
            contains: [
              hljs.APOS_STRING_MODE, hljs.QUOTE_STRING_MODE,
              hljs.CSS_NUMBER_MODE
            ]
          }
        ]
      },
      {
        className: 'selector-tag', begin: IDENT_RE,
        relevance: 0
      },
      {
        begin: '{', end: '}',
        illegal: /\S/,
        contains: [
          hljs.C_BLOCK_COMMENT_MODE,
          RULE,
        ]
      }
    ]
  };
};

/***/ }),
/* 8 */
/***/ (function(module, exports) {

module.exports = function(hljs) {
  return {
    aliases: ['patch'],
    contains: [
      {
        className: 'meta',
        relevance: 10,
        variants: [
          {begin: /^@@ +\-\d+,\d+ +\+\d+,\d+ +@@$/},
          {begin: /^\*\*\* +\d+,\d+ +\*\*\*\*$/},
          {begin: /^\-\-\- +\d+,\d+ +\-\-\-\-$/}
        ]
      },
      {
        className: 'comment',
        variants: [
          {begin: /Index: /, end: /$/},
          {begin: /={3,}/, end: /$/},
          {begin: /^\-{3}/, end: /$/},
          {begin: /^\*{3} /, end: /$/},
          {begin: /^\+{3}/, end: /$/},
          {begin: /\*{5}/, end: /\*{5}$/}
        ]
      },
      {
        className: 'addition',
        begin: '^\\+', end: '$'
      },
      {
        className: 'deletion',
        begin: '^\\-', end: '$'
      },
      {
        className: 'addition',
        begin: '^\\!', end: '$'
      }
    ]
  };
};

/***/ }),
/* 9 */
/***/ (function(module, exports) {

module.exports = function(hljs) {
  var BASIC_ATOM_RE = '[a-z\'][a-zA-Z0-9_\']*';
  var FUNCTION_NAME_RE = '(' + BASIC_ATOM_RE + ':' + BASIC_ATOM_RE + '|' + BASIC_ATOM_RE + ')';
  var ERLANG_RESERVED = {
    keyword:
      'after and andalso|10 band begin bnot bor bsl bzr bxor case catch cond div end fun if ' +
      'let not of orelse|10 query receive rem try when xor',
    literal:
      'false true'
  };

  var COMMENT = hljs.COMMENT('%', '$');
  var NUMBER = {
    className: 'number',
    begin: '\\b(\\d+#[a-fA-F0-9]+|\\d+(\\.\\d+)?([eE][-+]?\\d+)?)',
    relevance: 0
  };
  var NAMED_FUN = {
    begin: 'fun\\s+' + BASIC_ATOM_RE + '/\\d+'
  };
  var FUNCTION_CALL = {
    begin: FUNCTION_NAME_RE + '\\(', end: '\\)',
    returnBegin: true,
    relevance: 0,
    contains: [
      {
        begin: FUNCTION_NAME_RE, relevance: 0
      },
      {
        begin: '\\(', end: '\\)', endsWithParent: true,
        returnEnd: true,
        relevance: 0
        // "contains" defined later
      }
    ]
  };
  var TUPLE = {
    begin: '{', end: '}',
    relevance: 0
    // "contains" defined later
  };
  var VAR1 = {
    begin: '\\b_([A-Z][A-Za-z0-9_]*)?',
    relevance: 0
  };
  var VAR2 = {
    begin: '[A-Z][a-zA-Z0-9_]*',
    relevance: 0
  };
  var RECORD_ACCESS = {
    begin: '#' + hljs.UNDERSCORE_IDENT_RE,
    relevance: 0,
    returnBegin: true,
    contains: [
      {
        begin: '#' + hljs.UNDERSCORE_IDENT_RE,
        relevance: 0
      },
      {
        begin: '{', end: '}',
        relevance: 0
        // "contains" defined later
      }
    ]
  };

  var BLOCK_STATEMENTS = {
    beginKeywords: 'fun receive if try case', end: 'end',
    keywords: ERLANG_RESERVED
  };
  BLOCK_STATEMENTS.contains = [
    COMMENT,
    NAMED_FUN,
    hljs.inherit(hljs.APOS_STRING_MODE, {className: ''}),
    BLOCK_STATEMENTS,
    FUNCTION_CALL,
    hljs.QUOTE_STRING_MODE,
    NUMBER,
    TUPLE,
    VAR1, VAR2,
    RECORD_ACCESS
  ];

  var BASIC_MODES = [
    COMMENT,
    NAMED_FUN,
    BLOCK_STATEMENTS,
    FUNCTION_CALL,
    hljs.QUOTE_STRING_MODE,
    NUMBER,
    TUPLE,
    VAR1, VAR2,
    RECORD_ACCESS
  ];
  FUNCTION_CALL.contains[1].contains = BASIC_MODES;
  TUPLE.contains = BASIC_MODES;
  RECORD_ACCESS.contains[1].contains = BASIC_MODES;

  var PARAMS = {
    className: 'params',
    begin: '\\(', end: '\\)',
    contains: BASIC_MODES
  };
  return {
    aliases: ['erl'],
    keywords: ERLANG_RESERVED,
    illegal: '(</|\\*=|\\+=|-=|/\\*|\\*/|\\(\\*|\\*\\))',
    contains: [
      {
        className: 'function',
        begin: '^' + BASIC_ATOM_RE + '\\s*\\(', end: '->',
        returnBegin: true,
        illegal: '\\(|#|//|/\\*|\\\\|:|;',
        contains: [
          PARAMS,
          hljs.inherit(hljs.TITLE_MODE, {begin: BASIC_ATOM_RE})
        ],
        starts: {
          end: ';|\\.',
          keywords: ERLANG_RESERVED,
          contains: BASIC_MODES
        }
      },
      COMMENT,
      {
        begin: '^-', end: '\\.',
        relevance: 0,
        excludeEnd: true,
        returnBegin: true,
        lexemes: '-' + hljs.IDENT_RE,
        keywords:
          '-module -record -undef -export -ifdef -ifndef -author -copyright -doc -vsn ' +
          '-import -include -include_lib -compile -define -else -endif -file -behaviour ' +
          '-behavior -spec',
        contains: [PARAMS]
      },
      NUMBER,
      hljs.QUOTE_STRING_MODE,
      RECORD_ACCESS,
      VAR1, VAR2,
      TUPLE,
      {begin: /\.$/} // relevance booster
    ]
  };
};

/***/ }),
/* 10 */
/***/ (function(module, exports) {

module.exports = function(hljs) {
  return {
    keywords: {
      built_in:
        'spawn spawn_link self',
      keyword:
        'after and andalso|10 band begin bnot bor bsl bsr bxor case catch cond div end fun if ' +
        'let not of or orelse|10 query receive rem try when xor'
    },
    contains: [
      {
        className: 'meta', begin: '^[0-9]+> ',
        relevance: 10
      },
      hljs.COMMENT('%', '$'),
      {
        className: 'number',
        begin: '\\b(\\d+#[a-fA-F0-9]+|\\d+(\\.\\d+)?([eE][-+]?\\d+)?)',
        relevance: 0
      },
      hljs.APOS_STRING_MODE,
      hljs.QUOTE_STRING_MODE,
      {
        begin: '\\?(::)?([A-Z]\\w*(::)?)+'
      },
      {
        begin: '->'
      },
      {
        begin: 'ok'
      },
      {
        begin: '!'
      },
      {
        begin: '(\\b[a-z\'][a-zA-Z0-9_\']*:[a-z\'][a-zA-Z0-9_\']*)|(\\b[a-z\'][a-zA-Z0-9_\']*)',
        relevance: 0
      },
      {
        begin: '[A-Z][a-zA-Z0-9_\']*',
        relevance: 0
      }
    ]
  };
};

/***/ }),
/* 11 */
/***/ (function(module, exports) {

module.exports = function(hljs) {
  var VERSION = 'HTTP/[0-9\\.]+';
  return {
    aliases: ['https'],
    illegal: '\\S',
    contains: [
      {
        begin: '^' + VERSION, end: '$',
        contains: [{className: 'number', begin: '\\b\\d{3}\\b'}]
      },
      {
        begin: '^[A-Z]+ (.*?) ' + VERSION + '$', returnBegin: true, end: '$',
        contains: [
          {
            className: 'string',
            begin: ' ', end: ' ',
            excludeBegin: true, excludeEnd: true
          },
          {
            begin: VERSION
          },
          {
            className: 'keyword',
            begin: '[A-Z]+'
          }
        ]
      },
      {
        className: 'attribute',
        begin: '^\\w', end: ': ', excludeEnd: true,
        illegal: '\\n|\\s|=',
        starts: {end: '$', relevance: 0}
      },
      {
        begin: '\\n\\n',
        starts: {subLanguage: [], endsWithParent: true}
      }
    ]
  };
};

/***/ }),
/* 12 */
/***/ (function(module, exports) {

module.exports = function(hljs) {
  var IDENT_RE = '[A-Za-z$_][0-9A-Za-z$_]*';
  var KEYWORDS = {
    keyword:
      'in of if for while finally var new function do return void else break catch ' +
      'instanceof with throw case default try this switch continue typeof delete ' +
      'let yield const export super debugger as async await static ' +
      // ECMAScript 6 modules import
      'import from as'
    ,
    literal:
      'true false null undefined NaN Infinity',
    built_in:
      'eval isFinite isNaN parseFloat parseInt decodeURI decodeURIComponent ' +
      'encodeURI encodeURIComponent escape unescape Object Function Boolean Error ' +
      'EvalError InternalError RangeError ReferenceError StopIteration SyntaxError ' +
      'TypeError URIError Number Math Date String RegExp Array Float32Array ' +
      'Float64Array Int16Array Int32Array Int8Array Uint16Array Uint32Array ' +
      'Uint8Array Uint8ClampedArray ArrayBuffer DataView JSON Intl arguments require ' +
      'module console window document Symbol Set Map WeakSet WeakMap Proxy Reflect ' +
      'Promise'
  };
  var EXPRESSIONS;
  var NUMBER = {
    className: 'number',
    variants: [
      { begin: '\\b(0[bB][01]+)' },
      { begin: '\\b(0[oO][0-7]+)' },
      { begin: hljs.C_NUMBER_RE }
    ],
    relevance: 0
  };
  var SUBST = {
    className: 'subst',
    begin: '\\$\\{', end: '\\}',
    keywords: KEYWORDS,
    contains: []  // defined later
  };
  var TEMPLATE_STRING = {
    className: 'string',
    begin: '`', end: '`',
    contains: [
      hljs.BACKSLASH_ESCAPE,
      SUBST
    ]
  };
  SUBST.contains = [
    hljs.APOS_STRING_MODE,
    hljs.QUOTE_STRING_MODE,
    TEMPLATE_STRING,
    NUMBER,
    hljs.REGEXP_MODE
  ]
  var PARAMS_CONTAINS = SUBST.contains.concat([
    hljs.C_BLOCK_COMMENT_MODE,
    hljs.C_LINE_COMMENT_MODE
  ]);

  return {
    aliases: ['js', 'jsx'],
    keywords: KEYWORDS,
    contains: [
      {
        className: 'meta',
        relevance: 10,
        begin: /^\s*['"]use (strict|asm)['"]/
      },
      {
        className: 'meta',
        begin: /^#!/, end: /$/
      },
      hljs.APOS_STRING_MODE,
      hljs.QUOTE_STRING_MODE,
      TEMPLATE_STRING,
      hljs.C_LINE_COMMENT_MODE,
      hljs.C_BLOCK_COMMENT_MODE,
      NUMBER,
      { // object attr container
        begin: /[{,]\s*/, relevance: 0,
        contains: [
          {
            begin: IDENT_RE + '\\s*:', returnBegin: true,
            relevance: 0,
            contains: [{className: 'attr', begin: IDENT_RE, relevance: 0}]
          }
        ]
      },
      { // "value" container
        begin: '(' + hljs.RE_STARTERS_RE + '|\\b(case|return|throw)\\b)\\s*',
        keywords: 'return throw case',
        contains: [
          hljs.C_LINE_COMMENT_MODE,
          hljs.C_BLOCK_COMMENT_MODE,
          hljs.REGEXP_MODE,
          {
            className: 'function',
            begin: '(\\(.*?\\)|' + IDENT_RE + ')\\s*=>', returnBegin: true,
            end: '\\s*=>',
            contains: [
              {
                className: 'params',
                variants: [
                  {
                    begin: IDENT_RE
                  },
                  {
                    begin: /\(\s*\)/,
                  },
                  {
                    begin: /\(/, end: /\)/,
                    excludeBegin: true, excludeEnd: true,
                    keywords: KEYWORDS,
                    contains: PARAMS_CONTAINS
                  }
                ]
              }
            ]
          },
          { // E4X / JSX
            begin: /</, end: /(\/\w+|\w+\/)>/,
            subLanguage: 'xml',
            contains: [
              {begin: /<\w+\s*\/>/, skip: true},
              {
                begin: /<\w+/, end: /(\/\w+|\w+\/)>/, skip: true,
                contains: [
                  {begin: /<\w+\s*\/>/, skip: true},
                  'self'
                ]
              }
            ]
          }
        ],
        relevance: 0
      },
      {
        className: 'function',
        beginKeywords: 'function', end: /\{/, excludeEnd: true,
        contains: [
          hljs.inherit(hljs.TITLE_MODE, {begin: IDENT_RE}),
          {
            className: 'params',
            begin: /\(/, end: /\)/,
            excludeBegin: true,
            excludeEnd: true,
            contains: PARAMS_CONTAINS
          }
        ],
        illegal: /\[|%/
      },
      {
        begin: /\$[(.]/ // relevance booster for a pattern common to JS libs: `$(something)` and `$.something`
      },
      hljs.METHOD_GUARD,
      { // ES6 class
        className: 'class',
        beginKeywords: 'class', end: /[{;=]/, excludeEnd: true,
        illegal: /[:"\[\]]/,
        contains: [
          {beginKeywords: 'extends'},
          hljs.UNDERSCORE_TITLE_MODE
        ]
      },
      {
        beginKeywords: 'constructor', end: /\{/, excludeEnd: true
      }
    ],
    illegal: /#(?!!)/
  };
};

/***/ }),
/* 13 */
/***/ (function(module, exports) {

module.exports = function(hljs) {
  var LITERALS = {literal: 'true false null'};
  var TYPES = [
    hljs.QUOTE_STRING_MODE,
    hljs.C_NUMBER_MODE
  ];
  var VALUE_CONTAINER = {
    end: ',', endsWithParent: true, excludeEnd: true,
    contains: TYPES,
    keywords: LITERALS
  };
  var OBJECT = {
    begin: '{', end: '}',
    contains: [
      {
        className: 'attr',
        begin: /"/, end: /"/,
        contains: [hljs.BACKSLASH_ESCAPE],
        illegal: '\\n',
      },
      hljs.inherit(VALUE_CONTAINER, {begin: /:/})
    ],
    illegal: '\\S'
  };
  var ARRAY = {
    begin: '\\[', end: '\\]',
    contains: [hljs.inherit(VALUE_CONTAINER)], // inherit is a workaround for a bug that makes shared modes with endsWithParent compile only the ending of one of the parents
    illegal: '\\S'
  };
  TYPES.splice(TYPES.length, 0, OBJECT, ARRAY);
  return {
    contains: TYPES,
    keywords: LITERALS,
    illegal: '\\S'
  };
};

/***/ }),
/* 14 */
/***/ (function(module, exports) {

module.exports = function(hljs) {
  return {
    aliases: ['md', 'mkdown', 'mkd'],
    contains: [
      // highlight headers
      {
        className: 'section',
        variants: [
          { begin: '^#{1,6}', end: '$' },
          { begin: '^.+?\\n[=-]{2,}$' }
        ]
      },
      // inline html
      {
        begin: '<', end: '>',
        subLanguage: 'xml',
        relevance: 0
      },
      // lists (indicators only)
      {
        className: 'bullet',
        begin: '^([*+-]|(\\d+\\.))\\s+'
      },
      // strong segments
      {
        className: 'strong',
        begin: '[*_]{2}.+?[*_]{2}'
      },
      // emphasis segments
      {
        className: 'emphasis',
        variants: [
          { begin: '\\*.+?\\*' },
          { begin: '_.+?_'
          , relevance: 0
          }
        ]
      },
      // blockquotes
      {
        className: 'quote',
        begin: '^>\\s+', end: '$'
      },
      // code snippets
      {
        className: 'code',
        variants: [
          {
            begin: '^```\w*\s*$', end: '^```\s*$'
          },
          {
            begin: '`.+?`'
          },
          {
            begin: '^( {4}|\t)', end: '$',
            relevance: 0
          }
        ]
      },
      // horizontal rules
      {
        begin: '^[-\\*]{3,}', end: '$'
      },
      // using links - title and link
      {
        begin: '\\[.+?\\][\\(\\[].*?[\\)\\]]',
        returnBegin: true,
        contains: [
          {
            className: 'string',
            begin: '\\[', end: '\\]',
            excludeBegin: true,
            returnEnd: true,
            relevance: 0
          },
          {
            className: 'link',
            begin: '\\]\\(', end: '\\)',
            excludeBegin: true, excludeEnd: true
          },
          {
            className: 'symbol',
            begin: '\\]\\[', end: '\\]',
            excludeBegin: true, excludeEnd: true
          }
        ],
        relevance: 10
      },
      {
        begin: /^\[[^\n]+\]:/,
        returnBegin: true,
        contains: [
          {
            className: 'symbol',
            begin: /\[/, end: /\]/,
            excludeBegin: true, excludeEnd: true
          },
          {
            className: 'link',
            begin: /:\s*/, end: /$/,
            excludeBegin: true
          }
        ]
      }
    ]
  };
};

/***/ }),
/* 15 */
/***/ (function(module, exports) {

module.exports = function(hljs) {
  var COMMENT_MODE = hljs.COMMENT('--', '$');
  return {
    case_insensitive: true,
    illegal: /[<>{}*#]/,
    contains: [
      {
        beginKeywords:
          'begin end start commit rollback savepoint lock alter create drop rename call ' +
          'delete do handler insert load replace select truncate update set show pragma grant ' +
          'merge describe use explain help declare prepare execute deallocate release ' +
          'unlock purge reset change stop analyze cache flush optimize repair kill ' +
          'install uninstall checksum restore check backup revoke comment',
        end: /;/, endsWithParent: true,
        lexemes: /[\w\.]+/,
        keywords: {
          keyword:
            'abort abs absolute acc acce accep accept access accessed accessible account acos action activate add ' +
            'addtime admin administer advanced advise aes_decrypt aes_encrypt after agent aggregate ali alia alias ' +
            'allocate allow alter always analyze ancillary and any anydata anydataset anyschema anytype apply ' +
            'archive archived archivelog are as asc ascii asin assembly assertion associate asynchronous at atan ' +
            'atn2 attr attri attrib attribu attribut attribute attributes audit authenticated authentication authid ' +
            'authors auto autoallocate autodblink autoextend automatic availability avg backup badfile basicfile ' +
            'before begin beginning benchmark between bfile bfile_base big bigfile bin binary_double binary_float ' +
            'binlog bit_and bit_count bit_length bit_or bit_xor bitmap blob_base block blocksize body both bound ' +
            'buffer_cache buffer_pool build bulk by byte byteordermark bytes cache caching call calling cancel ' +
            'capacity cascade cascaded case cast catalog category ceil ceiling chain change changed char_base ' +
            'char_length character_length characters characterset charindex charset charsetform charsetid check ' +
            'checksum checksum_agg child choose chr chunk class cleanup clear client clob clob_base clone close ' +
            'cluster_id cluster_probability cluster_set clustering coalesce coercibility col collate collation ' +
            'collect colu colum column column_value columns columns_updated comment commit compact compatibility ' +
            'compiled complete composite_limit compound compress compute concat concat_ws concurrent confirm conn ' +
            'connec connect connect_by_iscycle connect_by_isleaf connect_by_root connect_time connection ' +
            'consider consistent constant constraint constraints constructor container content contents context ' +
            'contributors controlfile conv convert convert_tz corr corr_k corr_s corresponding corruption cos cost ' +
            'count count_big counted covar_pop covar_samp cpu_per_call cpu_per_session crc32 create creation ' +
            'critical cross cube cume_dist curdate current current_date current_time current_timestamp current_user ' +
            'cursor curtime customdatum cycle data database databases datafile datafiles datalength date_add ' +
            'date_cache date_format date_sub dateadd datediff datefromparts datename datepart datetime2fromparts ' +
            'day day_to_second dayname dayofmonth dayofweek dayofyear days db_role_change dbtimezone ddl deallocate ' +
            'declare decode decompose decrement decrypt deduplicate def defa defau defaul default defaults ' +
            'deferred defi defin define degrees delayed delegate delete delete_all delimited demand dense_rank ' +
            'depth dequeue des_decrypt des_encrypt des_key_file desc descr descri describ describe descriptor ' +
            'deterministic diagnostics difference dimension direct_load directory disable disable_all ' +
            'disallow disassociate discardfile disconnect diskgroup distinct distinctrow distribute distributed div ' +
            'do document domain dotnet double downgrade drop dumpfile duplicate duration each edition editionable ' +
            'editions element ellipsis else elsif elt empty enable enable_all enclosed encode encoding encrypt ' +
            'end end-exec endian enforced engine engines enqueue enterprise entityescaping eomonth error errors ' +
            'escaped evalname evaluate event eventdata events except exception exceptions exchange exclude excluding ' +
            'execu execut execute exempt exists exit exp expire explain export export_set extended extent external ' +
            'external_1 external_2 externally extract failed failed_login_attempts failover failure far fast ' +
            'feature_set feature_value fetch field fields file file_name_convert filesystem_like_logging final ' +
            'finish first first_value fixed flash_cache flashback floor flush following follows for forall force ' +
            'form forma format found found_rows freelist freelists freepools fresh from from_base64 from_days ' +
            'ftp full function general generated get get_format get_lock getdate getutcdate global global_name ' +
            'globally go goto grant grants greatest group group_concat group_id grouping grouping_id groups ' +
            'gtid_subtract guarantee guard handler hash hashkeys having hea head headi headin heading heap help hex ' +
            'hierarchy high high_priority hosts hour http id ident_current ident_incr ident_seed identified ' +
            'identity idle_time if ifnull ignore iif ilike ilm immediate import in include including increment ' +
            'index indexes indexing indextype indicator indices inet6_aton inet6_ntoa inet_aton inet_ntoa infile ' +
            'initial initialized initially initrans inmemory inner innodb input insert install instance instantiable ' +
            'instr interface interleaved intersect into invalidate invisible is is_free_lock is_ipv4 is_ipv4_compat ' +
            'is_not is_not_null is_used_lock isdate isnull isolation iterate java join json json_exists ' +
            'keep keep_duplicates key keys kill language large last last_day last_insert_id last_value lax lcase ' +
            'lead leading least leaves left len lenght length less level levels library like like2 like4 likec limit ' +
            'lines link list listagg little ln load load_file lob lobs local localtime localtimestamp locate ' +
            'locator lock locked log log10 log2 logfile logfiles logging logical logical_reads_per_call ' +
            'logoff logon logs long loop low low_priority lower lpad lrtrim ltrim main make_set makedate maketime ' +
            'managed management manual map mapping mask master master_pos_wait match matched materialized max ' +
            'maxextents maximize maxinstances maxlen maxlogfiles maxloghistory maxlogmembers maxsize maxtrans ' +
            'md5 measures median medium member memcompress memory merge microsecond mid migration min minextents ' +
            'minimum mining minus minute minvalue missing mod mode model modification modify module monitoring month ' +
            'months mount move movement multiset mutex name name_const names nan national native natural nav nchar ' +
            'nclob nested never new newline next nextval no no_write_to_binlog noarchivelog noaudit nobadfile ' +
            'nocheck nocompress nocopy nocycle nodelay nodiscardfile noentityescaping noguarantee nokeep nologfile ' +
            'nomapping nomaxvalue nominimize nominvalue nomonitoring none noneditionable nonschema noorder ' +
            'nopr nopro noprom nopromp noprompt norely noresetlogs noreverse normal norowdependencies noschemacheck ' +
            'noswitch not nothing notice notrim novalidate now nowait nth_value nullif nulls num numb numbe ' +
            'nvarchar nvarchar2 object ocicoll ocidate ocidatetime ociduration ociinterval ociloblocator ocinumber ' +
            'ociref ocirefcursor ocirowid ocistring ocitype oct octet_length of off offline offset oid oidindex old ' +
            'on online only opaque open operations operator optimal optimize option optionally or oracle oracle_date ' +
            'oradata ord ordaudio orddicom orddoc order ordimage ordinality ordvideo organization orlany orlvary ' +
            'out outer outfile outline output over overflow overriding package pad parallel parallel_enable ' +
            'parameters parent parse partial partition partitions pascal passing password password_grace_time ' +
            'password_lock_time password_reuse_max password_reuse_time password_verify_function patch path patindex ' +
            'pctincrease pctthreshold pctused pctversion percent percent_rank percentile_cont percentile_disc ' +
            'performance period period_add period_diff permanent physical pi pipe pipelined pivot pluggable plugin ' +
            'policy position post_transaction pow power pragma prebuilt precedes preceding precision prediction ' +
            'prediction_cost prediction_details prediction_probability prediction_set prepare present preserve ' +
            'prior priority private private_sga privileges procedural procedure procedure_analyze processlist ' +
            'profiles project prompt protection public publishingservername purge quarter query quick quiesce quota ' +
            'quotename radians raise rand range rank raw read reads readsize rebuild record records ' +
            'recover recovery recursive recycle redo reduced ref reference referenced references referencing refresh ' +
            'regexp_like register regr_avgx regr_avgy regr_count regr_intercept regr_r2 regr_slope regr_sxx regr_sxy ' +
            'reject rekey relational relative relaylog release release_lock relies_on relocate rely rem remainder rename ' +
            'repair repeat replace replicate replication required reset resetlogs resize resource respect restore ' +
            'restricted result result_cache resumable resume retention return returning returns reuse reverse revoke ' +
            'right rlike role roles rollback rolling rollup round row row_count rowdependencies rowid rownum rows ' +
            'rtrim rules safe salt sample save savepoint sb1 sb2 sb4 scan schema schemacheck scn scope scroll ' +
            'sdo_georaster sdo_topo_geometry search sec_to_time second section securefile security seed segment select ' +
            'self sequence sequential serializable server servererror session session_user sessions_per_user set ' +
            'sets settings sha sha1 sha2 share shared shared_pool short show shrink shutdown si_averagecolor ' +
            'si_colorhistogram si_featurelist si_positionalcolor si_stillimage si_texture siblings sid sign sin ' +
            'size size_t sizes skip slave sleep smalldatetimefromparts smallfile snapshot some soname sort soundex ' +
            'source space sparse spfile split sql sql_big_result sql_buffer_result sql_cache sql_calc_found_rows ' +
            'sql_small_result sql_variant_property sqlcode sqldata sqlerror sqlname sqlstate sqrt square standalone ' +
            'standby start starting startup statement static statistics stats_binomial_test stats_crosstab ' +
            'stats_ks_test stats_mode stats_mw_test stats_one_way_anova stats_t_test_ stats_t_test_indep ' +
            'stats_t_test_one stats_t_test_paired stats_wsr_test status std stddev stddev_pop stddev_samp stdev ' +
            'stop storage store stored str str_to_date straight_join strcmp strict string struct stuff style subdate ' +
            'subpartition subpartitions substitutable substr substring subtime subtring_index subtype success sum ' +
            'suspend switch switchoffset switchover sync synchronous synonym sys sys_xmlagg sysasm sysaux sysdate ' +
            'sysdatetimeoffset sysdba sysoper system system_user sysutcdatetime table tables tablespace tan tdo ' +
            'template temporary terminated tertiary_weights test than then thread through tier ties time time_format ' +
            'time_zone timediff timefromparts timeout timestamp timestampadd timestampdiff timezone_abbr ' +
            'timezone_minute timezone_region to to_base64 to_date to_days to_seconds todatetimeoffset trace tracking ' +
            'transaction transactional translate translation treat trigger trigger_nestlevel triggers trim truncate ' +
            'try_cast try_convert try_parse type ub1 ub2 ub4 ucase unarchived unbounded uncompress ' +
            'under undo unhex unicode uniform uninstall union unique unix_timestamp unknown unlimited unlock unpivot ' +
            'unrecoverable unsafe unsigned until untrusted unusable unused update updated upgrade upped upper upsert ' +
            'url urowid usable usage use use_stored_outlines user user_data user_resources users using utc_date ' +
            'utc_timestamp uuid uuid_short validate validate_password_strength validation valist value values var ' +
            'var_samp varcharc vari varia variab variabl variable variables variance varp varraw varrawc varray ' +
            'verify version versions view virtual visible void wait wallet warning warnings week weekday weekofyear ' +
            'wellformed when whene whenev wheneve whenever where while whitespace with within without work wrapped ' +
            'xdb xml xmlagg xmlattributes xmlcast xmlcolattval xmlelement xmlexists xmlforest xmlindex xmlnamespaces ' +
            'xmlpi xmlquery xmlroot xmlschema xmlserialize xmltable xmltype xor year year_to_month years yearweek',
          literal:
            'true false null',
          built_in:
            'array bigint binary bit blob boolean char character date dec decimal float int int8 integer interval number ' +
            'numeric real record serial serial8 smallint text varchar varying void'
        },
        contains: [
          {
            className: 'string',
            begin: '\'', end: '\'',
            contains: [hljs.BACKSLASH_ESCAPE, {begin: '\'\''}]
          },
          {
            className: 'string',
            begin: '"', end: '"',
            contains: [hljs.BACKSLASH_ESCAPE, {begin: '""'}]
          },
          {
            className: 'string',
            begin: '`', end: '`',
            contains: [hljs.BACKSLASH_ESCAPE]
          },
          hljs.C_NUMBER_MODE,
          hljs.C_BLOCK_COMMENT_MODE,
          COMMENT_MODE
        ]
      },
      hljs.C_BLOCK_COMMENT_MODE,
      COMMENT_MODE
    ]
  };
};

/***/ }),
/* 16 */
/***/ (function(module, exports) {

module.exports = function(hljs) {
  var XML_IDENT_RE = '[A-Za-z0-9\\._:-]+';
  var TAG_INTERNALS = {
    endsWithParent: true,
    illegal: /</,
    relevance: 0,
    contains: [
      {
        className: 'attr',
        begin: XML_IDENT_RE,
        relevance: 0
      },
      {
        begin: /=\s*/,
        relevance: 0,
        contains: [
          {
            className: 'string',
            endsParent: true,
            variants: [
              {begin: /"/, end: /"/},
              {begin: /'/, end: /'/},
              {begin: /[^\s"'=<>`]+/}
            ]
          }
        ]
      }
    ]
  };
  return {
    aliases: ['html', 'xhtml', 'rss', 'atom', 'xjb', 'xsd', 'xsl', 'plist'],
    case_insensitive: true,
    contains: [
      {
        className: 'meta',
        begin: '<!DOCTYPE', end: '>',
        relevance: 10,
        contains: [{begin: '\\[', end: '\\]'}]
      },
      hljs.COMMENT(
        '<!--',
        '-->',
        {
          relevance: 10
        }
      ),
      {
        begin: '<\\!\\[CDATA\\[', end: '\\]\\]>',
        relevance: 10
      },
      {
        begin: /<\?(php)?/, end: /\?>/,
        subLanguage: 'php',
        contains: [{begin: '/\\*', end: '\\*/', skip: true}]
      },
      {
        className: 'tag',
        /*
        The lookahead pattern (?=...) ensures that 'begin' only matches
        '<style' as a single word, followed by a whitespace or an
        ending braket. The '$' is needed for the lexeme to be recognized
        by hljs.subMode() that tests lexemes outside the stream.
        */
        begin: '<style(?=\\s|>|$)', end: '>',
        keywords: {name: 'style'},
        contains: [TAG_INTERNALS],
        starts: {
          end: '</style>', returnEnd: true,
          subLanguage: ['css', 'xml']
        }
      },
      {
        className: 'tag',
        // See the comment in the <style tag about the lookahead pattern
        begin: '<script(?=\\s|>|$)', end: '>',
        keywords: {name: 'script'},
        contains: [TAG_INTERNALS],
        starts: {
          end: '\<\/script\>', returnEnd: true,
          subLanguage: ['actionscript', 'javascript', 'handlebars', 'xml']
        }
      },
      {
        className: 'meta',
        variants: [
          {begin: /<\?xml/, end: /\?>/, relevance: 10},
          {begin: /<\?\w+/, end: /\?>/}
        ]
      },
      {
        className: 'tag',
        begin: '</?', end: '/?>',
        contains: [
          {
            className: 'name', begin: /[^\/><\s]+/, relevance: 0
          },
          TAG_INTERNALS
        ]
      }
    ]
  };
};

/***/ }),
/* 17 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "a", function() { return initialize; });
var HIGHLIGHT_CLASS = 'hll';

function onMouseEnter(evt) {
  var groupId = evt.target.getAttribute('data-group-id');
  var siblings = document.querySelectorAll('[data-group-id=\'' + groupId + '\']');

  for (var i = 0; i < siblings.length; ++i) {
    siblings[i].classList.add(HIGHLIGHT_CLASS);
  }
}

function onMouseLeave(evt) {
  var groupId = evt.target.getAttribute('data-group-id');
  var siblings = document.querySelectorAll('[data-group-id=\'' + groupId + '\']');

  for (var i = 0; i < siblings.length; ++i) {
    siblings[i].classList.remove(HIGHLIGHT_CLASS);
  }
}

function initialize() {
  var delims = document.querySelectorAll('[data-group-id]');

  for (var i = 0; i < delims.length; i++) {
    var elem = delims[i];
    elem.addEventListener('mouseenter', onMouseEnter);
    elem.addEventListener('mouseleave', onMouseLeave);
  }
}

/***/ }),
/* 18 */,
/* 19 */,
/* 20 */
/***/ (function(module, exports) {

/*
	MIT License http://www.opensource.org/licenses/mit-license.php
	Author Tobias Koppers @sokra
*/
// css base code, injected by the css-loader
module.exports = function(useSourceMap) {
	var list = [];

	// return the list of modules as css string
	list.toString = function toString() {
		return this.map(function (item) {
			var content = cssWithMappingToString(item, useSourceMap);
			if(item[2]) {
				return "@media " + item[2] + "{" + content + "}";
			} else {
				return content;
			}
		}).join("");
	};

	// import a list of modules into the list
	list.i = function(modules, mediaQuery) {
		if(typeof modules === "string")
			modules = [[null, modules, ""]];
		var alreadyImportedModules = {};
		for(var i = 0; i < this.length; i++) {
			var id = this[i][0];
			if(typeof id === "number")
				alreadyImportedModules[id] = true;
		}
		for(i = 0; i < modules.length; i++) {
			var item = modules[i];
			// skip already imported module
			// this implementation is not 100% perfect for weird media query combinations
			//  when a module is imported multiple times with different media queries.
			//  I hope this will never occur (Hey this way we have smaller bundles)
			if(typeof item[0] !== "number" || !alreadyImportedModules[item[0]]) {
				if(mediaQuery && !item[2]) {
					item[2] = mediaQuery;
				} else if(mediaQuery) {
					item[2] = "(" + item[2] + ") and (" + mediaQuery + ")";
				}
				list.push(item);
			}
		}
	};
	return list;
};

function cssWithMappingToString(item, useSourceMap) {
	var content = item[1] || '';
	var cssMapping = item[3];
	if (!cssMapping) {
		return content;
	}

	if (useSourceMap && typeof btoa === 'function') {
		var sourceMapping = toComment(cssMapping);
		var sourceURLs = cssMapping.sources.map(function (source) {
			return '/*# sourceURL=' + cssMapping.sourceRoot + source + ' */'
		});

		return [content].concat(sourceURLs).concat([sourceMapping]).join('\n');
	}

	return [content].join('\n');
}

// Adapted from convert-source-map (MIT)
function toComment(sourceMap) {
	// eslint-disable-next-line no-undef
	var base64 = btoa(unescape(encodeURIComponent(JSON.stringify(sourceMap))));
	var data = 'sourceMappingURL=data:application/json;charset=utf-8;base64,' + base64;

	return '/*# ' + data + ' */';
}


/***/ }),
/* 21 */
/***/ (function(module, exports, __webpack_require__) {

/*
	MIT License http://www.opensource.org/licenses/mit-license.php
	Author Tobias Koppers @sokra
*/

var stylesInDom = {};

var	memoize = function (fn) {
	var memo;

	return function () {
		if (typeof memo === "undefined") memo = fn.apply(this, arguments);
		return memo;
	};
};

var isOldIE = memoize(function () {
	// Test for IE <= 9 as proposed by Browserhacks
	// @see http://browserhacks.com/#hack-e71d8692f65334173fee715c222cb805
	// Tests for existence of standard globals is to allow style-loader
	// to operate correctly into non-standard environments
	// @see https://github.com/webpack-contrib/style-loader/issues/177
	return window && document && document.all && !window.atob;
});

var getTarget = function (target, parent) {
  if (parent){
    return parent.querySelector(target);
  }
  return document.querySelector(target);
};

var getElement = (function (fn) {
	var memo = {};

	return function(target, parent) {
                // If passing function in options, then use it for resolve "head" element.
                // Useful for Shadow Root style i.e
                // {
                //   insertInto: function () { return document.querySelector("#foo").shadowRoot }
                // }
                if (typeof target === 'function') {
                        return target();
                }
                if (typeof memo[target] === "undefined") {
			var styleTarget = getTarget.call(this, target, parent);
			// Special case to return head of iframe instead of iframe itself
			if (window.HTMLIFrameElement && styleTarget instanceof window.HTMLIFrameElement) {
				try {
					// This will throw an exception if access to iframe is blocked
					// due to cross-origin restrictions
					styleTarget = styleTarget.contentDocument.head;
				} catch(e) {
					styleTarget = null;
				}
			}
			memo[target] = styleTarget;
		}
		return memo[target]
	};
})();

var singleton = null;
var	singletonCounter = 0;
var	stylesInsertedAtTop = [];

var	fixUrls = __webpack_require__(22);

module.exports = function(list, options) {
	if (typeof DEBUG !== "undefined" && DEBUG) {
		if (typeof document !== "object") throw new Error("The style-loader cannot be used in a non-browser environment");
	}

	options = options || {};

	options.attrs = typeof options.attrs === "object" ? options.attrs : {};

	// Force single-tag solution on IE6-9, which has a hard limit on the # of <style>
	// tags it will allow on a page
	if (!options.singleton && typeof options.singleton !== "boolean") options.singleton = isOldIE();

	// By default, add <style> tags to the <head> element
        if (!options.insertInto) options.insertInto = "head";

	// By default, add <style> tags to the bottom of the target
	if (!options.insertAt) options.insertAt = "bottom";

	var styles = listToStyles(list, options);

	addStylesToDom(styles, options);

	return function update (newList) {
		var mayRemove = [];

		for (var i = 0; i < styles.length; i++) {
			var item = styles[i];
			var domStyle = stylesInDom[item.id];

			domStyle.refs--;
			mayRemove.push(domStyle);
		}

		if(newList) {
			var newStyles = listToStyles(newList, options);
			addStylesToDom(newStyles, options);
		}

		for (var i = 0; i < mayRemove.length; i++) {
			var domStyle = mayRemove[i];

			if(domStyle.refs === 0) {
				for (var j = 0; j < domStyle.parts.length; j++) domStyle.parts[j]();

				delete stylesInDom[domStyle.id];
			}
		}
	};
};

function addStylesToDom (styles, options) {
	for (var i = 0; i < styles.length; i++) {
		var item = styles[i];
		var domStyle = stylesInDom[item.id];

		if(domStyle) {
			domStyle.refs++;

			for(var j = 0; j < domStyle.parts.length; j++) {
				domStyle.parts[j](item.parts[j]);
			}

			for(; j < item.parts.length; j++) {
				domStyle.parts.push(addStyle(item.parts[j], options));
			}
		} else {
			var parts = [];

			for(var j = 0; j < item.parts.length; j++) {
				parts.push(addStyle(item.parts[j], options));
			}

			stylesInDom[item.id] = {id: item.id, refs: 1, parts: parts};
		}
	}
}

function listToStyles (list, options) {
	var styles = [];
	var newStyles = {};

	for (var i = 0; i < list.length; i++) {
		var item = list[i];
		var id = options.base ? item[0] + options.base : item[0];
		var css = item[1];
		var media = item[2];
		var sourceMap = item[3];
		var part = {css: css, media: media, sourceMap: sourceMap};

		if(!newStyles[id]) styles.push(newStyles[id] = {id: id, parts: [part]});
		else newStyles[id].parts.push(part);
	}

	return styles;
}

function insertStyleElement (options, style) {
	var target = getElement(options.insertInto)

	if (!target) {
		throw new Error("Couldn't find a style target. This probably means that the value for the 'insertInto' parameter is invalid.");
	}

	var lastStyleElementInsertedAtTop = stylesInsertedAtTop[stylesInsertedAtTop.length - 1];

	if (options.insertAt === "top") {
		if (!lastStyleElementInsertedAtTop) {
			target.insertBefore(style, target.firstChild);
		} else if (lastStyleElementInsertedAtTop.nextSibling) {
			target.insertBefore(style, lastStyleElementInsertedAtTop.nextSibling);
		} else {
			target.appendChild(style);
		}
		stylesInsertedAtTop.push(style);
	} else if (options.insertAt === "bottom") {
		target.appendChild(style);
	} else if (typeof options.insertAt === "object" && options.insertAt.before) {
		var nextSibling = getElement(options.insertAt.before, target);
		target.insertBefore(style, nextSibling);
	} else {
		throw new Error("[Style Loader]\n\n Invalid value for parameter 'insertAt' ('options.insertAt') found.\n Must be 'top', 'bottom', or Object.\n (https://github.com/webpack-contrib/style-loader#insertat)\n");
	}
}

function removeStyleElement (style) {
	if (style.parentNode === null) return false;
	style.parentNode.removeChild(style);

	var idx = stylesInsertedAtTop.indexOf(style);
	if(idx >= 0) {
		stylesInsertedAtTop.splice(idx, 1);
	}
}

function createStyleElement (options) {
	var style = document.createElement("style");

	if(options.attrs.type === undefined) {
		options.attrs.type = "text/css";
	}

	if(options.attrs.nonce === undefined) {
		var nonce = getNonce();
		if (nonce) {
			options.attrs.nonce = nonce;
		}
	}

	addAttrs(style, options.attrs);
	insertStyleElement(options, style);

	return style;
}

function createLinkElement (options) {
	var link = document.createElement("link");

	if(options.attrs.type === undefined) {
		options.attrs.type = "text/css";
	}
	options.attrs.rel = "stylesheet";

	addAttrs(link, options.attrs);
	insertStyleElement(options, link);

	return link;
}

function addAttrs (el, attrs) {
	Object.keys(attrs).forEach(function (key) {
		el.setAttribute(key, attrs[key]);
	});
}

function getNonce() {
	if (false) {}

	return __webpack_require__.nc;
}

function addStyle (obj, options) {
	var style, update, remove, result;

	// If a transform function was defined, run it on the css
	if (options.transform && obj.css) {
	    result = options.transform(obj.css);

	    if (result) {
	    	// If transform returns a value, use that instead of the original css.
	    	// This allows running runtime transformations on the css.
	    	obj.css = result;
	    } else {
	    	// If the transform function returns a falsy value, don't add this css.
	    	// This allows conditional loading of css
	    	return function() {
	    		// noop
	    	};
	    }
	}

	if (options.singleton) {
		var styleIndex = singletonCounter++;

		style = singleton || (singleton = createStyleElement(options));

		update = applyToSingletonTag.bind(null, style, styleIndex, false);
		remove = applyToSingletonTag.bind(null, style, styleIndex, true);

	} else if (
		obj.sourceMap &&
		typeof URL === "function" &&
		typeof URL.createObjectURL === "function" &&
		typeof URL.revokeObjectURL === "function" &&
		typeof Blob === "function" &&
		typeof btoa === "function"
	) {
		style = createLinkElement(options);
		update = updateLink.bind(null, style, options);
		remove = function () {
			removeStyleElement(style);

			if(style.href) URL.revokeObjectURL(style.href);
		};
	} else {
		style = createStyleElement(options);
		update = applyToTag.bind(null, style);
		remove = function () {
			removeStyleElement(style);
		};
	}

	update(obj);

	return function updateStyle (newObj) {
		if (newObj) {
			if (
				newObj.css === obj.css &&
				newObj.media === obj.media &&
				newObj.sourceMap === obj.sourceMap
			) {
				return;
			}

			update(obj = newObj);
		} else {
			remove();
		}
	};
}

var replaceText = (function () {
	var textStore = [];

	return function (index, replacement) {
		textStore[index] = replacement;

		return textStore.filter(Boolean).join('\n');
	};
})();

function applyToSingletonTag (style, index, remove, obj) {
	var css = remove ? "" : obj.css;

	if (style.styleSheet) {
		style.styleSheet.cssText = replaceText(index, css);
	} else {
		var cssNode = document.createTextNode(css);
		var childNodes = style.childNodes;

		if (childNodes[index]) style.removeChild(childNodes[index]);

		if (childNodes.length) {
			style.insertBefore(cssNode, childNodes[index]);
		} else {
			style.appendChild(cssNode);
		}
	}
}

function applyToTag (style, obj) {
	var css = obj.css;
	var media = obj.media;

	if(media) {
		style.setAttribute("media", media)
	}

	if(style.styleSheet) {
		style.styleSheet.cssText = css;
	} else {
		while(style.firstChild) {
			style.removeChild(style.firstChild);
		}

		style.appendChild(document.createTextNode(css));
	}
}

function updateLink (link, options, obj) {
	var css = obj.css;
	var sourceMap = obj.sourceMap;

	/*
		If convertToAbsoluteUrls isn't defined, but sourcemaps are enabled
		and there is no publicPath defined then lets turn convertToAbsoluteUrls
		on by default.  Otherwise default to the convertToAbsoluteUrls option
		directly
	*/
	var autoFixUrls = options.convertToAbsoluteUrls === undefined && sourceMap;

	if (options.convertToAbsoluteUrls || autoFixUrls) {
		css = fixUrls(css);
	}

	if (sourceMap) {
		// http://stackoverflow.com/a/26603875
		css += "\n/*# sourceMappingURL=data:application/json;base64," + btoa(unescape(encodeURIComponent(JSON.stringify(sourceMap)))) + " */";
	}

	var blob = new Blob([css], { type: "text/css" });

	var oldSrc = link.href;

	link.href = URL.createObjectURL(blob);

	if(oldSrc) URL.revokeObjectURL(oldSrc);
}


/***/ }),
/* 22 */
/***/ (function(module, exports) {


/**
 * When source maps are enabled, `style-loader` uses a link element with a data-uri to
 * embed the css on the page. This breaks all relative urls because now they are relative to a
 * bundle instead of the current page.
 *
 * One solution is to only use full urls, but that may be impossible.
 *
 * Instead, this function "fixes" the relative urls to be absolute according to the current page location.
 *
 * A rudimentary test suite is located at `test/fixUrls.js` and can be run via the `npm test` command.
 *
 */

module.exports = function (css) {
  // get current location
  var location = typeof window !== "undefined" && window.location;

  if (!location) {
    throw new Error("fixUrls requires window.location");
  }

	// blank or null?
	if (!css || typeof css !== "string") {
	  return css;
  }

  var baseUrl = location.protocol + "//" + location.host;
  var currentDir = baseUrl + location.pathname.replace(/\/[^\/]*$/, "/");

	// convert each url(...)
	/*
	This regular expression is just a way to recursively match brackets within
	a string.

	 /url\s*\(  = Match on the word "url" with any whitespace after it and then a parens
	   (  = Start a capturing group
	     (?:  = Start a non-capturing group
	         [^)(]  = Match anything that isn't a parentheses
	         |  = OR
	         \(  = Match a start parentheses
	             (?:  = Start another non-capturing groups
	                 [^)(]+  = Match anything that isn't a parentheses
	                 |  = OR
	                 \(  = Match a start parentheses
	                     [^)(]*  = Match anything that isn't a parentheses
	                 \)  = Match a end parentheses
	             )  = End Group
              *\) = Match anything and then a close parens
          )  = Close non-capturing group
          *  = Match anything
       )  = Close capturing group
	 \)  = Match a close parens

	 /gi  = Get all matches, not the first.  Be case insensitive.
	 */
	var fixedCss = css.replace(/url\s*\(((?:[^)(]|\((?:[^)(]+|\([^)(]*\))*\))*)\)/gi, function(fullMatch, origUrl) {
		// strip quotes (if they exist)
		var unquotedOrigUrl = origUrl
			.trim()
			.replace(/^"(.*)"$/, function(o, $1){ return $1; })
			.replace(/^'(.*)'$/, function(o, $1){ return $1; });

		// already a full url? no change
		if (/^(#|data:|http:\/\/|https:\/\/|file:\/\/\/|\s*$)/i.test(unquotedOrigUrl)) {
		  return fullMatch;
		}

		// convert the url to a full url
		var newUrl;

		if (unquotedOrigUrl.indexOf("//") === 0) {
		  	//TODO: should we add protocol?
			newUrl = unquotedOrigUrl;
		} else if (unquotedOrigUrl.indexOf("/") === 0) {
			// path should be relative to the base url
			newUrl = baseUrl + unquotedOrigUrl; // already starts with '/'
		} else {
			// path should be relative to current directory
			newUrl = currentDir + unquotedOrigUrl.replace(/^\.\//, ""); // Strip leading './'
		}

		// send back the fixed url(...)
		return "url(" + JSON.stringify(newUrl) + ")";
	});

	// send back the fixed css
	return fixedCss;
};


/***/ }),
/* 23 */,
/* 24 */,
/* 25 */,
/* 26 */,
/* 27 */,
/* 28 */,
/* 29 */,
/* 30 */,
/* 31 */,
/* 32 */,
/* 33 */,
/* 34 */,
/* 35 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(36);
module.exports = __webpack_require__(37);


/***/ }),
/* 36 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony import */ var highlight_js_lib_highlight__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(1);
/* harmony import */ var highlight_js_lib_highlight__WEBPACK_IMPORTED_MODULE_0___default = /*#__PURE__*/__webpack_require__.n(highlight_js_lib_highlight__WEBPACK_IMPORTED_MODULE_0__);
/* harmony import */ var highlight_js_lib_languages_bash__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(6);
/* harmony import */ var highlight_js_lib_languages_bash__WEBPACK_IMPORTED_MODULE_1___default = /*#__PURE__*/__webpack_require__.n(highlight_js_lib_languages_bash__WEBPACK_IMPORTED_MODULE_1__);
/* harmony import */ var highlight_js_lib_languages_css__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(7);
/* harmony import */ var highlight_js_lib_languages_css__WEBPACK_IMPORTED_MODULE_2___default = /*#__PURE__*/__webpack_require__.n(highlight_js_lib_languages_css__WEBPACK_IMPORTED_MODULE_2__);
/* harmony import */ var highlight_js_lib_languages_diff__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(8);
/* harmony import */ var highlight_js_lib_languages_diff__WEBPACK_IMPORTED_MODULE_3___default = /*#__PURE__*/__webpack_require__.n(highlight_js_lib_languages_diff__WEBPACK_IMPORTED_MODULE_3__);
/* harmony import */ var highlight_js_lib_languages_erlang__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(9);
/* harmony import */ var highlight_js_lib_languages_erlang__WEBPACK_IMPORTED_MODULE_4___default = /*#__PURE__*/__webpack_require__.n(highlight_js_lib_languages_erlang__WEBPACK_IMPORTED_MODULE_4__);
/* harmony import */ var highlight_js_lib_languages_erlang_repl__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(10);
/* harmony import */ var highlight_js_lib_languages_erlang_repl__WEBPACK_IMPORTED_MODULE_5___default = /*#__PURE__*/__webpack_require__.n(highlight_js_lib_languages_erlang_repl__WEBPACK_IMPORTED_MODULE_5__);
/* harmony import */ var highlight_js_lib_languages_http__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(11);
/* harmony import */ var highlight_js_lib_languages_http__WEBPACK_IMPORTED_MODULE_6___default = /*#__PURE__*/__webpack_require__.n(highlight_js_lib_languages_http__WEBPACK_IMPORTED_MODULE_6__);
/* harmony import */ var highlight_js_lib_languages_javascript__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(12);
/* harmony import */ var highlight_js_lib_languages_javascript__WEBPACK_IMPORTED_MODULE_7___default = /*#__PURE__*/__webpack_require__.n(highlight_js_lib_languages_javascript__WEBPACK_IMPORTED_MODULE_7__);
/* harmony import */ var highlight_js_lib_languages_json__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(13);
/* harmony import */ var highlight_js_lib_languages_json__WEBPACK_IMPORTED_MODULE_8___default = /*#__PURE__*/__webpack_require__.n(highlight_js_lib_languages_json__WEBPACK_IMPORTED_MODULE_8__);
/* harmony import */ var highlight_js_lib_languages_markdown__WEBPACK_IMPORTED_MODULE_9__ = __webpack_require__(14);
/* harmony import */ var highlight_js_lib_languages_markdown__WEBPACK_IMPORTED_MODULE_9___default = /*#__PURE__*/__webpack_require__.n(highlight_js_lib_languages_markdown__WEBPACK_IMPORTED_MODULE_9__);
/* harmony import */ var highlight_js_lib_languages_sql__WEBPACK_IMPORTED_MODULE_10__ = __webpack_require__(15);
/* harmony import */ var highlight_js_lib_languages_sql__WEBPACK_IMPORTED_MODULE_10___default = /*#__PURE__*/__webpack_require__.n(highlight_js_lib_languages_sql__WEBPACK_IMPORTED_MODULE_10__);
/* harmony import */ var highlight_js_lib_languages_xml__WEBPACK_IMPORTED_MODULE_11__ = __webpack_require__(16);
/* harmony import */ var highlight_js_lib_languages_xml__WEBPACK_IMPORTED_MODULE_11___default = /*#__PURE__*/__webpack_require__.n(highlight_js_lib_languages_xml__WEBPACK_IMPORTED_MODULE_11__);
/* harmony import */ var _makeup__WEBPACK_IMPORTED_MODULE_12__ = __webpack_require__(17);
// Dependencies
// ------------












 // Set up Highlight.js

highlight_js_lib_highlight__WEBPACK_IMPORTED_MODULE_0___default.a.configure({
  tabReplace: '    ',
  // 4 spaces
  languages: [] // disable auto-detect

});
highlight_js_lib_highlight__WEBPACK_IMPORTED_MODULE_0___default.a.registerLanguage('bash', highlight_js_lib_languages_bash__WEBPACK_IMPORTED_MODULE_1___default.a);
highlight_js_lib_highlight__WEBPACK_IMPORTED_MODULE_0___default.a.registerLanguage('css', highlight_js_lib_languages_css__WEBPACK_IMPORTED_MODULE_2___default.a);
highlight_js_lib_highlight__WEBPACK_IMPORTED_MODULE_0___default.a.registerLanguage('diff', highlight_js_lib_languages_diff__WEBPACK_IMPORTED_MODULE_3___default.a);
highlight_js_lib_highlight__WEBPACK_IMPORTED_MODULE_0___default.a.registerLanguage('erlang', highlight_js_lib_languages_erlang__WEBPACK_IMPORTED_MODULE_4___default.a);
highlight_js_lib_highlight__WEBPACK_IMPORTED_MODULE_0___default.a.registerLanguage('erlang-repl', highlight_js_lib_languages_erlang_repl__WEBPACK_IMPORTED_MODULE_5___default.a);
highlight_js_lib_highlight__WEBPACK_IMPORTED_MODULE_0___default.a.registerLanguage('http', highlight_js_lib_languages_http__WEBPACK_IMPORTED_MODULE_6___default.a);
highlight_js_lib_highlight__WEBPACK_IMPORTED_MODULE_0___default.a.registerLanguage('javascript', highlight_js_lib_languages_javascript__WEBPACK_IMPORTED_MODULE_7___default.a);
highlight_js_lib_highlight__WEBPACK_IMPORTED_MODULE_0___default.a.registerLanguage('json', highlight_js_lib_languages_json__WEBPACK_IMPORTED_MODULE_8___default.a);
highlight_js_lib_highlight__WEBPACK_IMPORTED_MODULE_0___default.a.registerLanguage('markdown', highlight_js_lib_languages_markdown__WEBPACK_IMPORTED_MODULE_9___default.a);
highlight_js_lib_highlight__WEBPACK_IMPORTED_MODULE_0___default.a.registerLanguage('sql', highlight_js_lib_languages_sql__WEBPACK_IMPORTED_MODULE_10___default.a);
highlight_js_lib_highlight__WEBPACK_IMPORTED_MODULE_0___default.a.registerLanguage('xml', highlight_js_lib_languages_xml__WEBPACK_IMPORTED_MODULE_11___default.a);
Object(_makeup__WEBPACK_IMPORTED_MODULE_12__[/* initialize */ "a"])();
highlight_js_lib_highlight__WEBPACK_IMPORTED_MODULE_0___default.a.initHighlightingOnLoad();

/***/ }),
/* 37 */
/***/ (function(module, exports, __webpack_require__) {


var content = __webpack_require__(38);

if(typeof content === 'string') content = [[module.i, content, '']];

var transform;
var insertInto;



var options = {"hmr":true}

options.transform = transform
options.insertInto = undefined;

var update = __webpack_require__(21)(content, options);

if(content.locals) module.exports = content.locals;

if(false) {}

/***/ }),
/* 38 */
/***/ (function(module, exports, __webpack_require__) {

exports = module.exports = __webpack_require__(20)(false);
// imports


// module
exports.push([module.i, "", ""]);

// exports


/***/ })
/******/ ]);