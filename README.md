# mdq: jq for Markdown

[![Code Coverage][coverage-shield]][coverage-link]
[![Pending TODOs][todos-shield]][todos-link]

[coverage-shield]: https://img.shields.io/endpoint?url=https%3A%2F%2Fgist.githubusercontent.com%2Fyshavit%2F53901a0115b596e015a891c41fb0f256%2Fraw%2Fmdq-coverage.json

[coverage-link]: https://github.com/yshavit/mdq/actions/workflows/coverage.yml?query=branch%3Amain

[todos-shield]: https://img.shields.io/endpoint?url=https%3A%2F%2Fgist.githubusercontent.com%2Fyshavit%2Fe7a9e9e72651da0d7d2b1fbbe56237d0%2Fraw%2Fmdq-todos.json

[todos-link]: https://github.com/search?q=repo%3Ayshavit%2Fmdq+NOT+path%3A.github%2Fworkflows%2Fcoverage.yml+NOT+path%3AREADME.md+todo&type=code

## What is mdq?

mdq aims to do for Markdown what jq does for JSON: provide an easy way to zero in on specific parts of a document.

For example, GitHub PRs are Markdown documents, and some organizations have specific templates with checklists for all
reviewers to complete. Enforcing these often requires ugly regexes that are a pain to write and worse to debug. Instead,
you can (for example) ask mdq for all uncompleted tasks:

```shell
mdq '- [ ]'
```

mdq is available under the MIT license. I am open to other permissive licenses, if you have one you prefer.

# Usage

Some examples to get you oriented, and then we'll dive into the details:

```shell
# Select sections containing "usage":
$ cat example.md | mdq '# usage'

# Select sections containing "usage", and within those find all unordered list items:
$ cat example.md | mdq '# usage | -'

# ... or maybe you only want the list items containing "note":
$ cat example.md | mdq '# usage | - note'
```

You can also output as JSON, which is particularly useful for feeding into jq:

```shell
# count uncompleted tasks
$ incomplete_tasks="$(cat example.md | mdq --json '- [ ]' | jq '.items | length')"
$ if [[ "$incomplete_tasks" -gt 0 ]]; then echo 'some tasks are incomplete!'; fi
```

## Quick tutorial

mdq treats Markdown as a stream of elements, which are then filtered down using selectors. Each selector selects one
kind of Markdown element (sections, list items, etc), optionally filtered by a string matcher you provide.

To use mdq, you just need to know what selectors are available, and how string matching works. The following four items
will probably get you 90% of what you need.

1. Individual selectors:

   There are just a handful of selector types you need to know about:
   ```bash
   $ cat example.md | mdq '# foo'       # find headers containing "foo"
   
   $ cat example.md | mdq '- foo'       # find unordered list items containing "foo"
   $ cat example.md | mdq '1. foo'      # find ordered list items containing "foo"
                                        #(note: the number must be exactly "1.")
   
   $ cat example.md | mdq '- [ ] foo'   # find uncompleted task items containing "foo"
   $ cat example.md | mdq '- [x] foo'   # find completed task items containing "foo"
   $ cat example.md | mdq '- [?] foo'   # find all task items containing "foo"
   
   $ cat example.md | mdq '[foo](bar)'  # find links with display text containing "foo"
                                        # and URL containing "bar"
   $ cat example.md | mdq '![foo](bar)' # ditto for images
   ```
2. String selection.

   In the above examples, we always looked for strings containing "foo" or "bar". You can match in other ways, too.
   Let's look at unordered lists as an example; all the "foo"s and "bar"s above work the same way:

   ```shell
   # Find all headers, regardless of their titles:
   $ cat example.md | mdq '#'   # an empty selector means "any"
   $ cat example.md | mdq '# *' # so does *; they're exactly equivalent
   
   # Find sections whose titles contain the text "unquoted strings".
   # Unquoted strings are case-insensitive
   $ cat example.md | mdq '# unquoted strings'
   
   # Find sections whose titles contain the text "quoted | strings".
   # (You'll see in a sec why the pipe char requires us to quote this.)
   # Quoted strings are case-sensitive.
   $ cat example.md | mdq '# "quoted | strings"'
   
   # Quoted strings can have escape sequences (unquoted strings can't)
   $ cat example.md | mdq '# "typical \n \"escape\" \u{2603} sequences" '
   
   # Anchors:
   $ cat example.md | mdq '# ^match start of string'
   $ cat example.md | mdq '# ^"match start of string"' # case-sensitive
   
   $ cat example.md | mdq '# match end of string$'
   $ cat example.md | mdq '# "match end of string"$' # case-sensitive
   
   $ cat example.md | mdq '# ^match full string$'
   
   # And no matching tool would be complete without:
   $ cat example.md | mdq '# /reg(ex|ular expressions)/'
   ```

3. Chaining selectors via pipe:
   ```shell
   # Find sections whose title contains "foo", and within those, find all unordered lists.
   $ cat example.md | mdq '# foo | - *
   ```
   This is why we needed to quote `"quoted | strings"` above. Without the quote, the matcher would end at `|` and
   interpet the input as two selectors:

    1. `# quoted`
    2. `strings`

   The first of those would look for sections whose title contains just `quoted` (without also checking for
   `| strings`). The second is in invalid selector.
4. JSON:

   The `--json` flag will cause mdq to output the selected items as JSON, instead of Markdown. The gory details
   are [below](#json), but if you just try it out, hopefully it's pretty intuitive.

That's most of what you need to know! The following is lots of detail.

## Full documentation

### Selectors

mdq's main feature is the **selector string**, which is a series of **selector**s delimited by pipes (`|`). Each
selector operates on a stream of elements, with the initial stream being a single-element stream that contains the
entire input document.

Selectors consist of a selector type and one or more **string matchers**. Selectors are designed to mirror the Markdown
they select.

The selector types are as follows (more details on each immediately below):

| Syntax                      | Selects              | String matcher matches...                                                          |
|-----------------------------|----------------------|------------------------------------------------------------------------------------|
| `# matcher`                 | section              | the section's header title.                                                        |
| `- matcher`                 | unordered list items | text in the list item                                                              |
| `- [?] matcher` (see below) | unordered task items | "                                                                                  |
| `1. matcher`                | ordered list items   | "                                                                                  |
| `- [?] matcher` (see below) | ordered task items   | "                                                                                  |
| `[matcher](matcher)`        | links                | two matchers: the first matches the link text, and the second matches the URL      |
| `![matcher](matcher)`       | images               | two matchers: the first matches the image alt text, and the second matches the URL |

String matchers can be:

- bareword strings
- quoted strings
- regular expressions
- empty (` `), which means "any"
- `*`, which also means "any" (if you prefer to write that out explicitly)

See [below](#string-matchers) for more details.

Taking the example above:

```bash
$ cat example.md | mdq '# usage | -'
```

This specifies two matchers:

- `# usage`, which is a section selector with `matcher=usage`; this looks for any section whose title contains "usage"
- `-`, which is an unordered list selector with an empty matcher; this looks for any list items

Piping these together yields: "any list item within a section whose title contains 'usage'."

The selector string is always UTF-8.

#### Section

```
# matcher
```

- The space between the `#` and the matcher is required, unless the matcher is empty.
- Selects sections whose header matches the matcher.
- The result is the section title and its contents, including any subsections.
- Does not differentiate between section levels: the markdown `# fizz buzz` and `## all the buzz` would _both_ match
  against `mdq '# buzz'` (discussion #120).

Examples:

- `#`: matches any section
- `# foo` matches any section whose title contains "foo"

#### Lists and tasks

```
- unordered list
- [?] unordered task
1. ordered list
1. [?] ordered task
```

- The space between the `-`/`1.` and the matcher is required, unless the matcher is empty.
- For ordered lists, the selector _must_ be `1.` exactly: no other numbers, and the period is required.
- Note that there is no way to specify "an ordered or unordered list item" (discussion #119).

For tasks (in either ordered or unordered form), there are three variants:

| variant | meaning                      |
|---------|------------------------------|
| `[ ]`   | match only uncompleted tasks |
| `[x]`   | match only completed tasks   |
| `[?]`   | match either                 |

Examples:

- `-`: matches any unordered list item
- `1.`: matches any ordered list item
- `- [?] todo`: matches any unordered task, regardless of whether it's completed or not, that contains the text "todo"

#### Links and images

```
[matcher](matcher)
![matcher](matcher)
```

- These work similarly, except one selects (only) links and the other selects (only) images.
- For the image selectors, no space is allowed between the `!` and the `[`.
- For both forms, the first matcher selects the link text or alt text (depending on the form), and the second matcher
  selects the URL.
- String anchors are especially useful for the URL matcher (see [below](#anchors)).

On output, mdq will normalize the markdown for links. See [below](#markdown-for-link-and-image-references) for details.

In Markdown, links and images can specify the URL in a few forms:

```markdown
- [as a reference][1]
- [inline](https://example.com)
- [collapsed][]  (GitHub extension)
- [shortcut]     (GitHub extension)

[1]: https://example.com/1

[collapsed]: https://example.com/collapsed

[shortcut]: https://example.com/shortcut
```

The URL matcher works identically on all of these, and always matches the URL. There is no way to match the reference
id (for example, `[1]` above).

Examples:

- `[]()`: matches all links
- `[hello]()`: matches all links whose display text contains "hello"
- `![](^https://example.com/)`: matches all images whose URL starts with `https://example.com/`

## String matchers

You can specify string matchers in several ways.

- The simplest is just an empty string (or any amount of whitespace), which means "any" and matches all elements.
- An asterisk (`*`) means the same thing, and any whitespace around it is also ignored.
- An unquoted string is just some text
- A quoted string, which starts with either double or single quotes (`"` or `'`).
- A regular expression, which starts with `/`.

Unquoted strings are case-insensitive, while quoted strings are case-sensitive. Both support [anchors](#anchors), as
described below. Both match any substring.

Regular expressions are case-sensitive by default, although the pattern syntax supports ignoring case. mdq doesn't
provide any special handling of anchors, though the pattern syntax supports them, just as you'd expect.

### Unquoted strings

An unquoted string:

- _must_ start with a letter
- has a context-specific ending delimiter, which in practice is hopefully pretty intuitive; see below.
    - has an additional ending delimiter `$`, which always applies (see [anchors](#anchors) below)
- doesn't support any escaping: every character until the ending delimiter is treated literally, except that leading and
  trailing whitespace is trimmed.
- is always case-insensitive
- matches any substring, unless you provide an anchor

The ending delimiter is just "the next token that completes this part of the selector":

- For section, list, and task selectors, this is the pipe (`|`) that delimits this selector from the next one.
- For link and image selectors:
    - For the display matcher (`[matcher]`), it's `]`
    - For the URL matcher (`(matcher)`), it's `)`

Example:

- `# some header text | -`: matcher is `some header text`.
    - note that the fact that it's in a section selector means that `|` is the ending delimiter
    - note also that the leading and trailing whitespace is trimmed
- `[foo]( example.com )`: first matcher is `foo` and second is `example.com`
    - note that the first matcher ends at the `]`, and the second at `)`. Hopefully that's pretty intuitive!
    - And again, note the trimmed whitespace. We could have also had it in the first matcher.
- `# hello, \u{2603}` matches literally `\​` `u` `{` `2` `6` `0` `3` `}`. It does _not_ match the [snowman]
  character.
- `# hello, ☃` does match the snowman
- `# fizz # buzz`: note that `-` here does not signify a second section selector, but is just a literal `#` in the
  matcher text.
- `# fizz$`: the `$` anchor is always a delimiter, so this matches "fizz" only at the end of a a section title

[snowman]: https://unicodesnowmanforyou.com

### Quoted strings

A quoted string:

- always starts with `"` or `'`, and always ends with that same character.
- is always case-sensitive
- matches any substring, unless you provide an anchor

Quoted strings support the following escape sequences:

| sequence         | produces                                                                                 |
|------------------|------------------------------------------------------------------------------------------|
| `\'`, `\"`       | `'` or `"`, respectively                                                                 |
| `\\`             | `\​`                                                                                     |
| `\n`, `\r`, `\t` | newline, carriage return, tab (respectively)                                             |
| `\u{...}`        | a single unicode code point; `...` is a hex number between 1 and 6 digits (respectively) |

- A double-quoted string may include single-quotes unescaped, and a single-quoted string may include double-quotes
  unescaped.
- `\'` and `\"` always work in both quoted string variants

Examples:

- `"hello's world"`
- `'my "hello world" message'`
- `'my \"hello world\" message'`: you don't need to escape the `"` (since we're in a `'`-quoted string), but you're
  allowed to
- `"A string with\nTwo lines"`: The `\n` here is a newline, not a literal `\​` `n`.
- `"I love \u{2603} in the winter"`: `\u{2603}` is a snowman: ☃
- `"I love \u2603 in the winter"`: syntax error, because the curly braces are always required for `\u` escapes

### Regexes

Regexes are always delimited by `/` (see discussion #56).

mdq uses the `regex` crate, so that's the syntax we support. In particular, it does not support negative lookaheads.
See: https://docs.rs/regex/1.10/regex/index.html#syntax, and issue #121 for negative lookaheads.

Regexes search for a match anywhere at the string, not just at the beginning. If you need to match just at the
beginning, use a `^` anchor within the pattern.

Examples:

- `/hello.+world/`: "hello", then one or more of any character, then "world"
- `/^hello/`: "hello", at the beginning of a string
- `/world$/`: "world", at the end of a string
- `/(?i)HELLO/`: "hello", case-insensitive (see [relevant section of the regex docs][regex-grouping])

[regex-grouping]: https://docs.rs/regex/1.10/regex/index.html#grouping-and-flags

### Anchors

Quoted and unquoted strings support the following anchors:

- `^`: anchors to start of string; must be first character
- `$`: anchors to end of string; must be the last character

Because of this anchor, `$` is always an ending delimiter for unquoted strings, regardless of the string's
context-specific delimiter.

For quoted strings, the anchors go immediately before or after the quotes. Whitespace is allowed between the anchor
and the quotes, and that whitespace is ignored.

For unquoted strings, whitespace is allowed before or after either anchor, and is trimmed away.

Examples:

- `^"foo"`: "foo" at the beginning of a string
- `"bar"$`: "bar" at the end of a string
- `^foobar$`: the string must be "foobar" exactly (since it's anchored to both the beginning and the end)
- `^    "foo"`, `"bar"    $`, `^ foobar $`: equivalent to the previous, respectively

## Output

### Markdown

By default, mdq will output all selected items as markdown. If there were multiple items selected, they will be
separated by a thematic break:

```markdown
   ---
```

You can also specify JSON output, as mentioned [below](#json).

#### Markdown for link and image references

- By default, all links and images will be converted to reference form.

  You can change this behavior with `--link_format`.
- By default, link references will be moved up to be in the first section that mentions the link:

  ```markdown
  # Section one

  A [link][1].

  [1]: https://example.com/1

  # Section two

  Note that the link reference (`[1]: `) went in section one, since that's the section that used it.
  ```

  You can change this behavior with `--link-placement`.

If converting links and images to reference form, all numeric references will be reordered to start at 1 and count
up sequentially from there. Any non-numeric references (for example, `[a]`) will be unaltered.

### JSON

If you specify `--json`, mdq will output its results as json. The schema is:

```json
{
  "items": [
    /* ... */
  ],
  "links": {
    "1": {
      "url": "<url>",
      "title": "[title]"
    }
  },
  "footnotes": {
    "a": [
      /* ... */
    ]
  }
}
```

- `"items"` contains an array of selected items. This entry is always present, but may be empty if there were no
  selected items. Each item is a polymorphic object; see below.
- `"links"` provides the reference definitions for links and images that use the `[text][1]` form _only_ for links and
  images that appear as part of inline text. (See warning below.)
    - The keys are the reference ids, without square brackets. For example, `1`.
    - The values are always objects, with either one or two entries:
        - `"url"`: the reference URL; always provided
        - `"title"`: the reference title, if one exists; omitted if there's no title.

          The title is often rendered as a tooltip, and is specified in markdown as:
          ```markdown
          [1]: https://example.com "this is a title"
          ```
    - `[collapsed][]` and `[shorthand]` links count as reference links, and will appear in this
      object.
    - `[inline](https://example.com)` links will _not_ be in this object.
    - If there are no reference-style links, the entire `"links"` entry will be omitted.
- `"footnotes"` provides the footnote texts for footnotes:
  ```markdown
  Some text that needs further explanation.[^a]
  
  [^a]: This is the footnote text. It is _full_ **markdown** and can
    - even
    - include block elements
  ```
    - The keys are the footnote ids, without the square brackets _or_ the `^`. In the example above, the id would
      be `"a"`.
    - The values follow the same syntax as the top-level `"items"`. Since footnote textmay contain several top-level
      blocks (as in the example above), this is an array.

> [!Warning]
>
> Links and images can appear in two different kinds of elements:
>
> - selected links / images, as by a `[]()` selector
> - inline markdown, such as in paragraphs: `some _text_ that [contains links][1]`
>
> The top-level `"links"` entry will only contain items for the second of these, the inline markdown. That's because
> for links and images you specifically select, the JSON object representing that item already has a place to put the
> link info; for inline markdown, there isn't such a place.
>
> This can be a bit jarring: you select a section that produces a `"links"` entry, but hen you select those links, that
> entry goes away:
>
> ```shell
> $ cat example.md | mdq --json '# my section'         # has top-level links entry
> $ cat example.md | mdq --json '# my section | []()'  # where did my links go??
> ```
>
> See discussion #123.

Each item is a single-element JSON object, whose key specifies what kind of item it is, and whose value depends on
the key. The following subsections will describe each one.

#### Document

This represents the full Markdown document; it's what you get if you run just `mdq --json` with no selectors.

```json
{
  "document": [
    /* items */
  ]
}
```

#### Section

```json
{
  "section": {
    "depth": 1,
    "title": "Markdown text. Inline elements like _emphasis_ or [links][1] are rendered as Markdown.",
    "body": [
      /* items */
    ]
  }
}
```

#### Paragraph

```json
{
  "paragraph": "Markdown text. Inline elements like _emphasis_ or [links][1] are rendered as Markdown."
}
```

#### Code block

~~~markdown
```rust metadata string
text of the code block
```
~~~

```json
{
  "code_block": {
    "code": "text of the code block",
    "type": "code | math | toml | yaml",
    "language": "rust",
    "metadata": "metadata string"
  }
}
```

- `language` and `metadata` are omitted if absent
- `metadata` is always absent if `language` is absent

#### Link

```json
{
  "link": {
    /* see below */
  }
}
```

Link item values always have a `"display"`, but the rest of the entries depend on the link style:

1. Reference links:
   ```markdown
   [Markdown text. Inline elements like _emphasis_ are rendered as Markdown][1]
   ```

   ```json
   {
     "display": "Markdown text. Inline elements like _emphasis_ are rendered as Markdown",
     "reference": "1"
   }
   ```

   (The top-level `"links"` entry will then contain the URL and title information.)
2. Inline links:
   ```markdown
   [Markdown _text_](https://example.com "link title")
   ```

   ```json
   {
     "display": "Markdown text. Inline elements like _emphasis_ or [links][1] are rendered as Markdown",
     "url": "https://example.com",
     "title": "link title"
   }
   ```
3. Collapsed or shortcut links:
   ```markdown
   [collapsed _markdown_][]
   
   [collapsed _markdown_]: https://example.com "link title"
   ```

   ```json
   {
     "display": "collapsed _markdown_",
     "url": "https://example.com",
     "title": "'link title' (omitted if absent)",
     "reference_style": "collapsed"
   }
   ```

   `[shortcut]` links work the same way, except that they have `"reference_style": "collapsed"`.

#### Image

Images work just like links, except that:

- the type specifier is `"image"`, not `"link`"
- `"display"` becomes `"alt"`

#### Block quote

```markdown
> Some
>
> Text
```

```json
{
  "block_quote": [
    /* items */
  ]
}
```

#### List

```markdown
1. one
2. two
```

```json
{
  "list": [
    /* list item values, as described below */
  ]
}
```

Each object is the _value_ of a list item object; it does not contain the enclosing `{"list_item": }` object. The
example would look like:

```json
{
  "list": [
    {
      "index": 1,
      "item": [
        {
          "paragraph": "one"
        }
      ]
    },
    {
      "index": 2,
      "item": [
        {
          "paragraph": "two"
        }
      ]
    }
  ]
}
```

#### List item

```markdown
3. [ ] text
```

```json
{
  "list_item": {
    "item": [
      /* items */
    ],
    "index": 3,
    "checked": false
  }
}
```

- List contents are blocks that can contain multiple segments, which is why `"item"` is an array.
- `"index"` is always an integer; it is omitted for unordered list items
- `"checked"` is always a boolean; it is omitted if the list item is not a task

#### Table

```markdown
| header 1 | header 2 |
|----------|----------|
| hello    | world    |
```

```json
{
  "table": {
    "alignments": [
      "left | right | center | none"
      /* ... */
    ],
    "rows": [
      [
        "header 1",
        "header 2"
      ],
      [
        "hello",
        "world"
      ]
    ]
  }
}
```

#### Thematic break

```markdown
   ----
```

```json
{
  "thematic_break": null
}
```