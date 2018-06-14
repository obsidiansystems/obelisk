set -e
set -o pipefail

REF=${1:-HEAD}

git ls-tree ${REF} | grep -v \.migration | git hash-object --stdin
