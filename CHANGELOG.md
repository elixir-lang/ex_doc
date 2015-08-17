# Changelog

## v0.8.4

* Bug fixes
  * Generate "README.html" file instead of "readme.html" as in previous releases
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
  * Update highlight.js with fixes some inlining issues
  * Require latest Earmark

## v0.7.2

* Bug fixes
  * Support Elixir master
  * Fix error reporting when modules are compiled without docs

## v0.7.1

* Enhancements
  * Use type=search for search input
  * Update highlight.js dependency
  * Properly tag code comments as coming from elixir/iex unless noted otherwise
  * Add support for hash redirection

## v0.7.0

* Enhancements
  * Documentation is now generated at doc to follow OTP "standard"

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
  * Add support for Earmark - no need for external processors

## v0.5.0

* Enhancements
  * First public release
  * Support pandoc and devinus/markdown as markdown processors
