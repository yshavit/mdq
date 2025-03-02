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
   brew install mdq
   ```
   (Mac and Linux)
1. ```bash
   docker pull yshavit/mdq
   echo 'My [example](https://github.com/yshavit/mdq) markdown' | docker run --rm -i yshavit/mdq '[]()'
   ```
1. Download binaries from [the latest release] (or any other release, of course).

   - Macs quarantine downloads from the internet by default. If you get an error saying that Apple cannot check the the binary for malicious software, you can remove this flag by running the following on the binary after extracting it from the artifact zip:
     ```bash
     xattr -d com.apple.quarantine mdq
     ```
   - You can also grab the binaries from the latest [build-release] workflow run. You must be logged into GitHub to do
     that (this is GitHub's limitation, not mine). You'll have to `chmod +x` them before you can run them.
1. ```shell
   cargo install --git https://github.com/yshavit/mdq
   ```
   Requires rustc >= 1.78.0

<details>
<summary>Security concerns</summary>
The release and latest-workflow binaries are built on GitHub's servers, so if you trust my code (and dependencies), and you trust GitHub,
you can trust the binaries. See https://github.com/yshavit/mdq/wiki/Release-binaries for information on how to verify them.
 </details>

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
the resulting element is of a different shape than the original. See the example below, or the wiki for more detail.)

In any of the above, the text may be:

- an `unquoted string` that starts with a letter; this is case-insensitive
- a `"quoted string"` (either single or double quotes); this is case-sensitive
- a string (quoted or unquoted) anchored by `^` or `$` (for start and end of string, respectively)
- a `/regex/`
- omitted or `*`, to mean "any"

See the [tutorial] for a bit more detail, and [user manual] for the full picture.

[tutorial]: https://github.com/yshavit/mdq/wiki/Tutorial

[user manual]: https://github.com/yshavit/mdq/wiki/Full-User-Manual

## Examples

### Ensuring that people have searched existing issues before submitting a bug report

Many projects have bug report templates that ask the submitter to attest that they've checked existing issues for possible duplicates. In mdq, you can do:

```bash
if echo "$ISSUE_TEXT" | mdq -q '- [x] I have searched for existing issues' ; then
  ...
```

(The `-q` option is like grep's: it doesn't output anything to stdout, but exits 0 if any items were found, or non-0 otherwise.)

This will match:

> - [x] I have searched for existing issues

... but will fail if the checkbox is unchecked:

> - [ ] I have searched for existing issues

### Extracting a referenced ticket

Some organizations use GitHub Actions to update their ticket tracker, if a PR mentions a ticket. You can use mdq to extract the link from Markdown as JSON, and then use jq to get the URL:

```bash
TICKET_URL="$(echo "$PR_TEXT"
  | mdq --output json '# Ticket | [](^https://tickets.example.com/[A-Z]+-\d+$)'
  | jq -r '.items[].link.url')"
```

This will match Markdown like:

> #### Ticket
>
> https://tickets.example.com/PROJ-1234

### Whittling down a big table

Let's say you have a table whose columns reference people in an on-call schedule, rows correspond to weeks in `YYYY-MM-DD` format:

> |   On-Call  | Alice | Bob | Sam | Pat |
> |:----------:|:-----:|:---:|:---:|:---:|
> | 2024-01-08 |   x   |     |     |     |
> | 2024-01-15 |       |     |  x  |     |
> | 2024-01-22 |       | x   |     |     |

To find out when Alice is on call:

```bash
cat oncall.md | mdq ':-: /On-Call|Alice/:-: *'
```
```markdown
|  On-Call   | Alice |
|:----------:|:-----:|
| 2024-01-08 |   x   |
| 2024-01-15 |       |
| 2024-01-22 |       |
```

Or, to find out who's on call for the week of Jan 15:

```bash
cat oncall.md | mdq ':-: * :-: 2024-01-15'
```
```markdown
|  On-Call   | Alice | Bob | Sam | Pat |
|:----------:|:-----:|:---:|:---:|----:|
| 2024-01-15 |       |     |  x  |     |
```

# Development

Requires rustc >= 1.78.0

```bash
cargo build
```

```bash
cargo test
```
