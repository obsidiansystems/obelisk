# Calculate vertex hash for the "obelisk-upgrade" migration graph.
#
# First argument is the directory to compute the hash for.
# Second optional argument specifies the specific git revision of that directory
# (repository).

set -e
set -o pipefail

REPO=${1:-.}
REV=${2:-HEAD}
NUMARGS=$#

# Files to ignore when calculating hash.
EXCLUDE="^migration"

# List of all files in current directory
#
# This excludes directories. If in a git repo, it will leverage `git ls-files`
# while ensuring that no untracked files exist. If elsewhere (possibly an
# exported git repo archive) it uses good ol' `find`.
function getFiles {
    if [ -d ".git" ]; then
        if [[ $(git ls-files . --exclude-standard --others) ]]; then
            # This warning is mainly to prevent the developer from accidentally
            # ignoring a new file yet to be added to git index.
            >&2 echo "ERROR: Untracked files (show below) cannot exist when calculating hash"
            >&2 git ls-files . --exclude-standard --others
            exit 1;
        fi;
        if [ $NUMARGS -lt 2 ]; then
            # Working copy files
            # >&2 echo "did NOT specify rev"
            git ls-files
        else
            # >&2 echo "specified rev ${REV}"
            git ls-tree -r --name-only ${REV}
        fi
    else
        # XXX: the exit here doesn't exit the script
        [ $NUMARGS -lt 2 ] || (>&2 echo "ERROR: Cannot pass rev when not in git repo"; exit 2;);
        find . -not -type d -printf '%P\n'
    fi | grep -v ${EXCLUDE} | sort;
}

# Read a path, returning target of symlink as its content.
function readLocalPath {
    FILEPATH=$1
    if [[ -L "$FILEPATH" ]]; then
        # Symlink content from `git show` doesn't include a newline; so we must
        # strip the newline here.
        readlink $FILEPATH | tr -d '\n'
    else
        cat $FILEPATH
    fi
}

# Read the given file
#
# This reads from the git revision *if* $2 (revision) was passed.
function readFile {
    FILEPATH=$1
    if [ -d ".git" ]; then
        if [ $NUMARGS -lt 2 ]; then
            # Working copy only.
            readLocalPath $FILEPATH
        else
            git show ${REV}:${FILEPATH}
        fi
    else
        readLocalPath $FILEPATH
    fi
}

# Run md5sum on stdin while using the given filepath to tag it.
function MD5SumStdin {
    FILEPATH=$1
    md5sum | python -c "import sys; print sys.stdin.read().replace('-', '''$FILEPATH'''),"
}


# Compute the md5sum of all files and then compute the md5sum of their output combined.
pushd ${REPO} > /dev/null
for f in `getFiles`; do
    readFile $f | MD5SumStdin $f
done | md5sum | awk '{print $1}'
