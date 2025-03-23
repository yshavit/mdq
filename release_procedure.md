# Cutting new releases

## tldr

1. run [prepare-new-release], which will create a PR and draft release
2. wait for CIs to succeed on the PR
3. validate the release (instructions will be on the PR)
4. do a fast-forward commit
5. immediately merge the subsequent PR that gets created (a different one from step 1)

## Details

1. Run the [prepare-new-release] action.

   To illustrate the values to pass to that, let's say:

    - `main` is currently at `v0.1.2-dev`
    - you want to cut a release named `v0.2.0`
    - after the release, you want `main` to be on version `v0.2.1-dev`.

   Select the following options:

   | Option                                         | Value    |
   |------------------------------------------------|----------|
   | version to cut a release as                    | `v0.2.0` |
   | the new dev to prepare (w/o "-dev" suffix)     | `v0.2.1` |
   | the git ref to go against                      | `main`   |
   | verify current dev version (w/o "-dev" suffix) | `v0.1.2` |

2. A workflow will create a PR to update `v0.1.2-dev` to `v0.2.0`, and create a release for that PR's sha in draft
   mode. The PR's branch will be named `pending-releases/v0.2.0`

    1. The [release-assets] workflow will:

        1. Validate that the PR has a linear history to main with just one additional commit.
        2. Use the [build-release] workflow to build the binaries
        3. Upload the artifacts to the draft PR.

    2. If you need to make any changes (like adding a new commit), rebase and force-push to `pending-releases/v0.2.0`.

        - The branch should always be against main, and have exactly one extra commit.
        - The release-assets workflow will re-run each time you update the PR.

3. Follow the instructions in the PR description to validate it.

4. When you're satisfied with the PR, merge it via a fast-forward commit. This must be done locally.

   ```bash
   git fetch
   git checkout main
   git pull origin main
   git merge --ff-only origin/pending-releases/v0.2.0
   git push
   ```

5. Another workflow ([post-release]) will publish the release and create a new PR to bump the version to `v0.2.1-dev`

   > [!important]
   > Merge this PR immediately! Even if there are any CI failures, just merge it. (It's okay to hit retries on the
   > CIs if you need to, but don't do any other code change.)

That's it!

[prepare-new-release]: https://github.com/yshavit/mdq/actions/workflows/prepare-new-release.yml

[release-assets]: https://github.com/yshavit/mdq/actions/workflows/release-assets.yml

[build-release]: https://github.com/yshavit/mdq/actions/workflows/build-release.yml

[post-release]: https://github.com/yshavit/mdq/actions/workflows/post-release.yml
