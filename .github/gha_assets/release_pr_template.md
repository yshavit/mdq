Bump version to **{{RELEASE_VERSION}}** for release.

## Validation:

- on one of the binaries:
  - [ ] `mdq --version`
  - [ ] `gh attestation verify -o yshavit mdq`

- docker image:
    - [ ] `docker pull yshavit/mdq:{{RELEASE_VERSION}}-rc`
    - [ ] `docker run --rm -i yshavit/mdq:{{RELEASE_VERSION}} --version-rc`

- [ ] review release notes
 
