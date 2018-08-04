# Changelog

## v0.19.1

  * Enhancements
    * Update CSS styles
    * Remove sourcemaps from package

## v0.19.0

This release requires Elixir v1.7 and later. For earlier Elixir versions, use ExDoc ~> 0.18.0.

  * Enhancements
    * Do not select "iex>" when selecting code samples
    * Use makeup to perform ELixir's syntax highlighting (other languages still use highlight.js)
    * Use `[rel="noopener"] on external links
    * Link directly to page if sidebar item has no subitems
    * Autolink Kernel and Kernel functions and special forms with shorthands (for example, only `is_atom/1` is necessary)
    * Trim EEx contents to reduce HTML size
    * Allow apps to be excluded from umbrella app docs

  * Bug fixes
    * Exclude types with `@typedoc false` from the docs
    * Make sure autolink considers the longest matching dependency name in case of shared prefixes

## v0.18.3

  * Bug fix
    * Fix formatting of typespecs causing errors
    * Update jQuery
    * Properly remove underscore from typespec links

## v0.18.2

  * Enhancements
    * Improve documentation pages for printing
    * Autolink Kernel, Kernel.SpecialForms, and built-in types
    * Annotate opaque types
    * Add vertical-align:top to tables
    * Allow module-function-arity references in links, for example: [some code](`MyMod.fun/2`)
    * Remove underscore from view source link
    * Run code formatter on typespecs (if one is available)
    * Make night mode switch link more obvious

## v0.18.1

  * Bug fixes
    * Include missing formatter files

## v0.18.0

  * Enhancements
    * No longer strip empty parens from types
    * Provide more extension point for markdown processors
    * Remove assets from priv since they are now embedded at compile time

  * Backwards incompatible changes
    * Remove built-in support for hoedown markdown processor
    * No longer add favicon when logo option is present (this was added in 0.17.0 but it was reported the logo almost never fits as a favicon)

## v0.17.1

  * Bug fixes
    * Fix broken search caused by outdated JavaScript files

## v0.17.0

  * Enhancements
    * Allow modules to be grouped on the sidebar with the `:groups_for_modules` option
    * Allow extras to be grouped on the sidebar with the `:groups_for_extras` option

  * Backwards incompatible changes
    * The previous `:group` option that could be specified for each entry in `:extras` is no longer supported
    * No longer add a specific section for "Protocols". If you would like to segregate the protocols in your application, use the new `:groups_for_modules` functionality

## v0.16.4

  * Enhancements
    * Generate favicon link if a logo is available

  * Bug fixes
    * Do not version epub filename as the doc directory is likely already versioned

## v0.16.3

  * Enhancements
    * Make sure its own search.html page and provide history
    * Generate source maps only if the `--debug` flag is given
    * Users can now add custom HTML before the closing head tag and the closing body tag
    * Highlight the target function/macro/type/callback when clicked

  * Bug fixes
    * Remove extra `term()` argument at start of macro spec
    * Add unencoded link target for functions with special chars to cope with different behaviour in browsers

## v0.16.2

  * Enhancements
    * Link `<h3>` headers as well
    * Add border to code tag
  * Bug fixes
    * Fix sidebar on mobile devices

## v0.16.1

  * Bug fixes
    * Fix hover icons coloring

## v0.16.0

  * Enhancements
    * Separate tasks into "Mix Tasks" in sidebar
    * Add types to the search results
    * Improve accessibility

  * Bug fixes
    * Strip nesting HTML tags in h2 headers
    * Remove the old search results after every new search attempt

## v0.15.1

  * Bug fixes
    * Improve project name wrapping on HTML
    * Properly set link in types with parameter
    * Fix ExDoc.CLI.main/2 to keep --source-ref on Elixir v1.4
    * Do not fail if localStorage is not available

## v0.15.0

  * Enhancements
    * Closing the search takes you to where you were in the previous page
    * Handle `__struct__/0` by displaying the struct instead
    * Warn when redirecting to a non-existent file
    * List both functions and macros under "Functions"
    * Automatically detect GitLab source URL patterns

  * Bug fixes
    * Break long specs into lines
    * Fix the initial state of the sidebar hamburger
    * Do not error when abstract code is missing
    * Properly link to erlang lib dirs

  * Backwards incompatible changes
    * No longer support Pandoc
    * Require Earmark 1.1

## v0.14.5

  * Enhancements
    * Allow ExDoc to work as an escript

## v0.14.4

  * Enhancements
    * Point to Elixir docs on hexdocs.pm
    * Many improvements to layout and styling of EPUB formatter
    * Support multiple formatters to be configured on `mix.exs`
    * Also digest `sidebar_items.js`
    * Force parentheses on type signature to remove ambiguity
    * Generate top-level docs for an umbrella project
    * Searching on mobile closes menu when the Enter key is hit

## v0.14.3

  * Enhancements
    * Support the `:assets` option that automatically copies all entries in the given directory to `doc/assets`
    * Remove numbering on Extras subheaders from sidebar
    * Pass file and line information to markdown formatters for better warnings
    * Allow extra pages to be grouped together under a given heading
    * Generate ids for default name/arity pairs so they can be linked (both remotely and locally)

  * Bug fixes
    * Fix autolink for functions containing `%`, `{`, `}` or uppercase characters in the name

## v0.14.2

  * Enhancements
    * Automatically generate documentations links to any of your dependencies (by default links to hexdocs.pm but allows the URL to be configured)
    * Allow documentation to be generated to Erlang modules

  * Bug fixes
    * Make sure "Top" is not included twice on pages

## v0.14.1

  * Bug fixes
    * Include "Top" link in pages with no headings
    * Include "Top" link in modules with no docs

## v0.14.0

  * Enhancements
    * Add support for the epub formatter
    * Support extraction from `<h2>` headers out of the settext format

  * Layout changes
    * Indent documentation inside the function/macro/type/callback header
    * Style types the same way as function/macro/callback and include a link to the source
    * Increase font-sizes in the sidebar and code snippets
    * Move the specs definition inside the function/macro/type/callback header and make better use of vertical space
    * Use a gradient on the sidebar to avoid sudden cut-off when scrolling the modules list
    * Fix the use of the back-button in some browsers
    * Allow the whole sidebar to be navigated without moving away from the current page. Expand (+) and collapse (-) buttons have been added to aid exploration
    * Numerically order pages/guides headers

## v0.13.2

  * Bug fixes
    * Avoid scrollbar from appearing on all code snippets

## v0.13.1

  * Enhancements
    * Autolink Elixir's stdlib modules and functions
    * Show callbacks in search results
    * Reduce size taken by font-sizes
    * Increase size for headings in the moduledoc

  * Bug fixes
    * Fix opened sidebar on small screens
    * Allow horizontal scrolling for code snippets on small screens

## v0.13.0

  * Bug fixes
    * Fix issue where docs would fail when being built on Erlang 19
    * Store templates in `priv` rather than in `lib`

  * Backwards incompatible changes
    * Require Elixir ~> v1.2

## v0.12.0

  * Enhancements
    * Remove warnings when running on Elixir v1.3
    * Support for `@optional_callbacks`
    * Improve styling for nested lists
    * Include earmark as a default dependency

  * Bug fixes
    * Fix many styling and performance front-end related bugs

## v0.11.5

  * Enhancements
    * Support canonical URLs

  * Bug fixes
    * Autolink now allows digits in function names
    * Sort specs by line to preserve ordering
    * Focus on content element on document ready
    * Remove ligature fix on Firefox and Safari as Google Fonts have been updated

## v0.11.4

  * Bug fixes
    * Fix ligature issues in recent browsers
    * HTML escape headers
    * Avoid warning on Elixir master (v1.3)

## v0.11.3

  * Bug fixes
    * Fix a regression where the sidebar wouldn't stick on small screens

## v0.11.2

  * Enhancements
    * Include night mode for docs
    * Take advantage of extra space on large screens by widening sidebar

  * Bug fixes
    * Do not attempt to retrieve docs from Erlang modules

## v0.11.1

  * Bug fixes
    * Include callbacks even if a module defines a struct

## v0.11.0

  * Enhancements
    * From now on it's possible to set the title in the sidebar area for
      additional content, *default:* "Pages"
    * Set the path and title of each additional page in `mix.exs` file
    * Use the first `h1` as menu title if title is not configured
    * Include the project name as part of the header in small devices

  * Bug fixes
    * Increase the visual separation between functions
    * Remove the `extra-` prefix for the additional documentation files
    * Extra large images do not create an overflow in the content

## v0.10.0

  * Enhancements
    * Many improvements and bug fixes in new layout
    * Reduced build size
    * Overview has been renamed to API Reference
    * All extra content, including API Reference, has been moved to inside
      "Pages"
    * Extra files are now downcased and prefixed by `extra-`

## v0.9.0

  * Enhancements
    * Whole new clean, readable, usable, responsive layout
    * Support for adding logo to generated docs (must be 64x64 pixels)
    * Support for adding extra pages to generated docs
    * Improve formatting of typespecs and callbacks

  * Backwards incompatible changes
    * `--readme` option and `:readme` configuration have been removed. Use
      `:extras` in your `mix.exs` file or pass `--extra` / `-e` in the
      command-line (may be given multiple times)

## v0.8.4

  * Bug fixes
    * Generate `README.html` file instead of `readme.html` as in previous
      releases
    * Style fixes in the new layout

## v0.8.3

  * Bug fixes
    * Style fixes in the new layout

## v0.8.2

  * Enhancements
    * Uglify and minify JS and CSS code
    * Performance improvements when building sidebar
    * Redirect from index.html to proper page

  * Bug fixes
    * Style fixes in the new layout

## v0.8.1

  * Bug fixes
    * Style fixes in the new layout

## v0.8.0

  * Enhancements
    * New and responsive layout without frames

## v0.7.3

  * Bug fixes
    * Update [highlight.js][] with fixes some inlining issues
    * Require latest [Earmark][]

## v0.7.2

  * Bug fixes
    * Support Elixir master
    * Fix error reporting when modules are compiled without docs

## v0.7.1

  * Enhancements
    * Use `type=search` for search input
    * Update [highlight.js][] dependency
    * Properly tag code comments as coming from Elixir/IEx unless noted otherwise
    * Add support for hash redirection

## v0.7.0

  * Enhancements
    * Documentation is now generated at `doc` to follow OTP "standard"

## v0.6.2

  * Enhancements
    * Improvements to the document structure
    * Add syntax highlight

## v0.6.1

  * Enhancements
    * Autolink modules and functions in the README
    * Generate ids for callbacks starting with "c:"
    * Ensure group ordering is consistent: TYPES > FUNCTIONS > MACROS > CALLBACKS
    * Allow users to search by Module.function

## v0.6.0

  * Enhancements
    * Support Elixir v1.0.0-rc1

## v0.5.2

  * Bug fixes
    * Use proper ANSI escape sequence on Mix success messages

## v0.5.1

  * Enhancements
    * Support Elixir v0.15.0
    * Add support for [Earmark][] - no need for external processors

## v0.5.0

  * Enhancements
    * First public release
    * Support [pandoc][] and [devinus/markdown][markdown] as markdown processors

[pandoc]: http://pandoc.org/
[markdown]: https://github.com/devinus/markdown
[earmark]: https://github.com/pragdave/earmark
[highlight.js]: https://highlightjs.org/
