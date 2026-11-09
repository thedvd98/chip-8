# Source this file (bash) to use the project-local egg repository:
#
#   . scripts/env.sh
#
# Eggs are installed into .chicken/eggs, so nothing is written to the
# (read-only) Guix store or to the user's home.

CHIP8_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
export CHIP8_ROOT

# Remember the repository path set by the environment (e.g. by Guix) only
# once, so sourcing this file more than once does not keep growing it.
if [ -z "${CHIP8_ORIG_REPOSITORY_PATH+set}" ]; then
  export CHIP8_ORIG_REPOSITORY_PATH="${CHICKEN_REPOSITORY_PATH:-}"
fi

# Built-in repository of the chicken in PATH: holds the core modules
# (chicken.base, srfi-4, ...) and must stay in the search path.
CHIP8_CHICKEN_CORE_REPO="$(env -u CHICKEN_REPOSITORY_PATH -u CHICKEN_INSTALL_REPOSITORY chicken-install -repository)"

export CHICKEN_INSTALL_REPOSITORY="$CHIP8_ROOT/.chicken/eggs"
CHICKEN_REPOSITORY_PATH="$CHICKEN_INSTALL_REPOSITORY"
if [ -n "$CHIP8_ORIG_REPOSITORY_PATH" ]; then
  CHICKEN_REPOSITORY_PATH="$CHICKEN_REPOSITORY_PATH:$CHIP8_ORIG_REPOSITORY_PATH"
fi
export CHICKEN_REPOSITORY_PATH="$CHICKEN_REPOSITORY_PATH:$CHIP8_CHICKEN_CORE_REPO"

mkdir -p "$CHICKEN_INSTALL_REPOSITORY"
