# mdq: jq for Markdown

[![Code Coverage][coverage-shield]][coverage-link]
[![Build status][build-shield]][build-link]
[![Pending TODOs][todos-shield]][todos-link]
[![Ignored tests][ignoreds-shield]][ignoreds-link]

[coverage-shield]: https://img.shields.io/endpoint?url=https%3A%2F%2Fgist.githubusercontent.com%2Fyshavit%2F53901a0115b596e015a891c41fb0f256%2Fraw%2Fmdq-coverage.json

[coverage-link]: https://github.com/yshavit/mdq/actions/workflows/coverage.yml?query=branch%3Amain

[build-shield]: https://github.com/yshavit/mdq/actions/workflows/rust.yml/badge.svg

[build-link]: https://github.com/yshavit/mdq/actions/workflows/rust.yml?query=branch%3Amain

[todos-shield]: https://img.shields.io/endpoint?url=https%3A%2F%2Fgist.githubusercontent.com%2Fyshavit%2Fe7a9e9e72651da0d7d2b1fbbe56237d0%2Fraw%2Fmdq-todos.json

[todos-link]: https://github.com/search?q=repo%3Ayshavit%2Fmdq+NOT+path%3A.github%2Fworkflows%2Fcoverage.yml+NOT+path%3AREADME.md+todo&type=code

[ignoreds-shield]: https://img.shields.io/endpoint?url=https%3A%2F%2Fgist.githubusercontent.com%2Fyshavit%2F782a8dc5f77d2cf4b1c774da72636f00%2Fraw%2Fmdq-ignoreds.json

[ignoreds-link]: https://github.com/search?q=repo%3Ayshavit%2Fmdq+%28%28path%3A%2F%5C.rs%24%2F+%22%23%5Bignore%5D%22%29+OR+%28path%3Atests%2Fmd_cases+%2F%5Eignore%2F%29%29&type=code

## What is mdq?

mdq aims to do for Markdown what jq does for JSON: provide an easy way to zero in on specific parts of a document.

For example, GitHub PRs are Markdown documents, and some organizations have specific templates with checklists for all
reviewers to complete. Enforcing these often requires ugly regexes that are a pain to write and worse to debug. Instead,
you can (for example) ask mdq for all uncompleted tasks:

```shell
mdq '- [ ]'
```

mdq is available under the Apache 2.0 or MIT licenses, at your option. I am open to other permissive licenses, if you
have one you prefer.

# Installation

Any of these will work:

1. ```shell
   cargo install --git https://github.com/yshavit/mdq
   ```
2. You can download binaries from [the latest release] (or any other release, of course).
3. You can also grab the binaries from the latest [build-release] workflow run. You must be logged into GitHub to do
   that (their limitation, not mine!)

> [!Tip]
> - These binaries are all built on GitHub's servers, so if you trust my code (and dependencies), and you trust GitHub,
    > you can trust the binaries.
    >
    >   See [the wiki page on release binaries] for information on how to verify them.
> - You'll have to `chmod +x` them before you can run them.

[the wiki page on release binaries]: https://github.com/yshavit/mdq/wiki/Release-binaries

[the latest release]: https://github.com/yshavit/mdq/releases/latest

[build-release]: https://github.com/yshavit/mdq/actions/workflows/build-release.yml

# Basic Usage

Simple example to select sections containing "usage":

```shell
cat example.md | mdq '# usage'
```

Use pipe (`|`) to chain filters together. For example, to select sections containing "usage", and within those find
all unordered list items:

```shell
cat example.md | mdq '# usage | -'
```

The filter syntax is designed to mirror Markdown syntax. You can select...

| Element          | Syntax                           |
|------------------|----------------------------------|
| Sections         | `# title text`                   |
| Lists            | `- unordered list item text`     |
| "                | `1. ordered list item text`      |
| "                | `- [ ] uncompleted task`         |
| "                | `- [x] completed task`           |
| "                | `- [?] any task`                 |
| Links            | `[display text](url)`            |
| Images           | `![alt text](url)`               |
| Block quotes     | `> block quote text`             |
| Code blocks      | ` ```language <code block text>` |
| Raw HTML         | `</> html_tag`                   |
| Plain paragraphs | `P: paragraph text `             |
| Tables           | `:-: header text :-: row text`   |

(Tables selection differs from other selections in that you can actually select only certain headers and rows, such that
the resulting element is of a different shape than the original. See the wiki for more.)

In any of the above, the text may be:

- an `unquoted string` that starts with a letter; this is case-insensitive
- a `"quoted string"` (either single or double quotes); this is case-sensitive
- a string (quoted or unquoted) anchored by `^` or `$` (for start and end of string, respectively)
- a `/regex/`
- omitted or `*`, to mean "any"

See the [tutorial] for a bit more detail, and [user manual] for the full picture.

[tutorial]: https://github.com/yshavit/mdq/wiki/Tutorial

[user manual]: https://github.com/yshavit/mdq/wiki/Full-User-Manual
