set -e
set -o pipefail

REPO=${1:-.}
REF=${2:-HEAD}

git -C ${REPO} ls-tree ${REF} | grep -v migration$ | git hash-object --stdin
