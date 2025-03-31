# Cutting new releases

## tldr

1. run [prepare-new-release], which will create a PR and draft release
2. follow the instructions on the PR
3. run [release-publish]
4. check that the PR that gets created (a different one from step 1) is merged; if not, merge it immediately

## Details

1. Run the [prepare-new-release] action.

   To illustrate the values to pass to that, let's say:

    - `main` is currently at `v0.1.2-dev`
    - you want to cut a release named `v0.2.0`

   Select the following options:

   | Option                                         | Value    |
   |------------------------------------------------|----------|
   | version to cut a release as                    | `v0.2.0` |
   | the git ref to go against                      | `main`   |
   | verify current dev version (w/o "-dev" suffix) | `v0.1.2` |

   A workflow will create a PR to update `v0.1.2-dev` to `v0.2.0` and create a release for that PR's sha in draft
   mode. The PR's branch will be named `pending-releases/v0.2.0`.

    1. The [release-assets] workflow will:

        1. Validate that the PR has a linear history to main with just one additional commit.
        2. Use the [build-release] workflow to build the binaries
        3. Upload the artifacts to the draft PR.
        4. Publish a docker image with a tag `0.2.0-rc`

    2. If you need to make any changes (like adding a new commit), rebase and force-push to `pending-releases/v0.2.0`.

        - The branch should always be against main, and have exactly one extra commit.
        - The release-assets workflow will re-run each time you update the PR.

2. Follow the instructions in the PR description to validate it.

3. When you're satisfied with the PR, run the [release-publish] action. This will:

   1. Validate that the PR's tasks are all complete
   2. On Docker Hub, re-tag the `0.2.0-rc` image to `0.2.0`.
   3. Do a fast-forward merge of the PR. (The fast-forward ensures that the commit SHA in main for the `0.2.0` release
      matches the commit sha that the binaries and docker were built against.)
   4. Open up a new PR for the next `-dev` version bump

4. Make sure the new PR gets merged immediately. The automation should do this for you; but if it doesn't, do it now.

[prepare-new-release]: https://github.com/yshavit/mdq/actions/workflows/prepare-new-release.yml

[release-assets]: https://github.com/yshavit/mdq/actions/workflows/release-assets.yml

[build-release]: https://github.com/yshavit/mdq/actions/workflows/build-release.yml

[release-publish]: https://github.com/yshavit/mdq/actions/workflows/release-publish.yml
    