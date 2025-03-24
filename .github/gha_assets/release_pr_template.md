Bump version to **{{RELEASE_VERSION}}** for release.

Validation:

- linux image:
  - [ ] `bash mdq --version`
  - [ ] `gh attestation verify -o yshavit mdq`

- windows image:
    - [ ] `bash mdq --version`
    - [ ] `gh attestation verify -o yshavit mdq`

- mac image:
    - [ ] `mdq --version`
    - [ ] `gh attestation verify -o yshavit mdq`

- docker image:
    - [ ] `docker pull yshavit/mdq:{{RELEASE_VERSION}}`
    - [ ] `docker run --rm -i yshavit/mdq:{{RELEASE_VERSION}} --version`

- [ ] review release notes