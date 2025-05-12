---
title: My example
---

# Greetings

![welcome image](https://example.com/welcome.png)

How are you!

*I'm* doing well.

## Hello there

In fact, I'm doing very well!
This line isn't a new paragraph, just some
wrapping that mdq should unwrap.

### Sub-section with link

This is [my referenced link][a1].

[a1]: https://example.com/reference

# Details

### Here's a *cool* table

| Column Left | Column Middle | Column Right |
|:------------|:-------------:|-------------:|
| (           |       v       |            ) |

This is [my inline link](https://example.com/inline).

This is [my inline link with title](https://example.com/inline "its title").

This is [my referenced link with title][a2].

This is [a collapsed link][], and this is [a shortcut link].

[a collapsed link]: https://example.com/collapsed
[a shortcut link]: https://example.com/shortcut "and it has a title"

## Hello lists

1. List one
2. List two with...

   ...two paragraphs

- Item a[^1]

- [x] checked
- [ ] unchecked and contains a [link within a task](https://example.com/task)

[a2]: https://example.com/reference "from the previous section"
[^1]: interesting footnote

> This is so great

## Foo

> I say:
> ```types
> here is some code
> it is
> 
> fun
> ```

There's also a

```text title="Code block with metadata"
Block A
```

and

``` title="Code block with only metadata"
Block B
```

# Second Section

## Heading `with inline code` again

1. countit
2. [ ] fizz

- maybe
- [x] yes _but_ maybe not
- [ ] no
  ```
  foo
  bar
  ```

- again

## List details

1. One
1.
    2. Two
1. s
1. Four
1. ```
   foo
   ```
1.

```
foo
```

## Html Stuff

First some <span>inline</span>.

<div class="then some"
href="#block-level">

With paragraph text between it.

</div>
