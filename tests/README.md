# Integration tests

Test cases are in `md_cases/` and are defined by toml files with the following format:

```toml
[given]
md = '''
Some input markdown
'''

[expect."test case name"]
cli_args = ["arguments", "passed", "to", "cli"]
output = '''
The expected output
'''

[expect."another text case"]
# ...
```

[`build.rs`] compiles these into test functions that construct an `integ_test::Case` and invoke its [`fn check`]. Each
file becomes a `mod`, and each case becomes a test fn within that `mod`.

[`build.rs`]: ../build.rs

[`fn check`]: integ_test.rs