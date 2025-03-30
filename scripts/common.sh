set -euo pipefail

group_level=0

function echo_with_grouping() {
  for _ in $(seq 1 "$group_level"); do
    printf '  '
  done
  if [[ "${GITHUB_ACTIONS-}" ]]; then
    echo "$@"
  else
    echo >&2 -e "$@"
  fi
}

function group() {
  local title="$1"
  if [[ "${GITHUB_ACTIONS-}" ]]; then
    echo "::group::$title"
  else
    echo_with_grouping "${title//[^a-zA-Z0-9 _\'\"]/ /}"
    group_level="$((group_level + 1))"
  fi
}

function end_group() {
  if [[ "${GITHUB_ACTIONS-}" ]]; then
    echo "::endgroup::"
  else
    group_level="$((group_level - 1))"
  fi
}

function msg () {
  case "$#" in
    1)
      local level=info
      local title=''
      local message="$1"
      ;;
    2)
      local level=info
      local title="$1"
      local message="$2"
      ;;
    *)
      local level="$1"
      local title="$2"
      local message="$3"
      ;;
  esac
  if [[ "${GITHUB_ACTIONS-}" ]]; then
    case "$level" in
      debug)
        echo "::debug::$title: ${message}"
        ;;
      notice|warning|error)
        echo "::$level title=${title}::${message}"
        ;;
      *)
        if [[ -n "$title" ]]; then
          echo "$title: ${message}"
        else
          echo "${message}"
        fi
        ;;
    esac
  else
    local color=''
    local color_title_end=''
    local after_title=''
    case "$level" in
      debug)
        color='\e[37m'
        ;;
      notice)
        color='\e[34m'
        color_title_end='\e[0m'
        ;;
      warning)
        color='\e[33m'
        color_title_end='\e[0m'
        ;;
      error)
        color='\e[31m'
        color_title_end='\e[0m'
        ;;
      *)
        color='\e[0m'
        ;;
    esac
    if [[ -n "$title" ]]; then
      after_title=': '
    fi

    echo_with_grouping "${color}${title}${color_title_end}${after_title}${message}\e[0m"
  fi
}

function msg_debug() {
  msg debug "$@"
}

function msg_info() {
  msg notice "$@"
}

function err() {
  msg "$@"
  exit 1
}

function require_tool() {
  local tool="$1"
  command -v "$tool" &>/dev/null || {
    err 'missing software' "$tool"
  }
}

