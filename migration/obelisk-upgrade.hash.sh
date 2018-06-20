set -e
set -o pipefail

REPO=${1:-.}

pushd ${REPO}


# TODO: use `find` when not a git repo
# List of all files in current directory
#
# This returns the files only (not directories or symlinks). If in a git repo,
# it will leverage `git ls-files` while ensuring that no untracked files exist.
# If elsewhere (possibly an exported git repo archive) it uses `find`.
function getFiles () {
    if [ -d ".git" ]; then
        if [[ $(git ls-files . --exclude-standard --others) ]]; then
            >&2 echo "ERROR: Untracked files (show below) cannot exist when calculating hash"
            >&2 git ls-files . --exclude-standard --others
            exit 1;
        fi;
        git ls-files | grep -v ^migration | sort
    else
        find . -type f -printf '%P\n' | grep -v ^migration | sort
    fi;
}

# Compute the md5sum of all files and then compute the md5sum of their output combined.
for f in `getFiles`; do
    test -f $f && md5sum $f;
done | md5sum | awk '{print $1}'
