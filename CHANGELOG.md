# Changelog

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
