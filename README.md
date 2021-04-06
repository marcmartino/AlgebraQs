# Algebra Qs ![Tests](https://github.com/marcmartino/AlgebraQs/actions/workflows/test.yml/badge.svg)

Web app for answering algebra questions like "What is 3 \* 12?" or "What's seven times two minus six?"

Available to view at https://algebraQs.marmar.io

### Built with

- `elm/parser`, `mdgriffith/elm-ui`, `elm-explorations/test`
- hosted on Github Pages with preview instances via Netlify

### Points of Interest

As this is a project created for the sake of learning, not actually for utility sake I've compiled a list of bits that were relatively new to me as someone making their first large(ish) elm app

- `elm/parser` turned out to be fantastic for parsing the input string and turning it into an object representing the algebraic statement that the user wants solved. This was solvable without the use of back tracking.
- `mdgriffith/elm-ui` has been a fascinating library for doing layouts with much more clarity/sanity than standard html/css.
- Flags and ports for [javascript interop](https://github.com/marcmartino/AlgebraQs/blob/master/src/index.js) to enable manipulation of the favicon and storage of the theme (light mode or dark mode).
- Commands for browser url manipulation and history push as well as [random number generation](https://github.com/marcmartino/AlgebraQs/blob/master/src/ExampleStatementGenerator.elm).
