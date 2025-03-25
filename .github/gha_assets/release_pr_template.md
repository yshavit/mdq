Bump version to **{{RELEASE_VERSION}}** for release.

## Validation:

- linux image:
  - [ ] `mdq --version`
  - [ ] `gh attestation verify -o yshavit mdq`

- windows image:
    - [ ] `mdq --version`
    - [ ] `gh attestation verify -o yshavit mdq`

- mac image:
    - [ ] `mdq --version`
    - [ ] `gh attestation verify -o yshavit mdq`

- docker image:
    - [ ] `docker pull yshavit/mdq:{{RELEASE_VERSION}}-rc`
    - [ ] `docker run --rm -i yshavit/mdq:{{RELEASE_VERSION}} --version-rc`

- [ ] review release notes
 
