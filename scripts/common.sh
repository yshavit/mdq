set -euo pipefail

function msg() {
  echo >&2 "$@"
}

function err() {
  msg "$@"
  exit 1
}
