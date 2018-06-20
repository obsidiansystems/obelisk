#!/bin/sh

set -e
set -o pipefail

EDGESDIR=migration/edges
TMPVERTICES=$(mktemp)
TMPEDGES=$(mktemp)


# Find hashes for each git commit, and create vertices list.
(for ref in `git log --abbrev-commit --pretty=oneline | awk '{print $1}'`; do
     >&2 echo "Reading $ref"
     migration/obelisk-upgrade.hash.sh . $ref;
 done) | uniq | perl -ne 'print if ++$k{$_}==1' > $TMPVERTICES

echo "Created vertices at $TMPVERTICES"

# Create edge pairs
cat $TMPVERTICES | python -c "import sys; inp=[s.strip() for s in sys.stdin.readlines()]; inp.reverse(); pairs=zip(inp, inp[1:]); print '\n'.join([a + '-' + b for (a,b) in pairs])" > $TMPEDGES

echo "Created edges at $TMPEDGES"

# Create the migration graph store
for e in `cat $TMPEDGES`; do
    mkdir -p $EDGESDIR/$e;
    touch $EDGESDIR/$e/obelisk-upgrade;
    touch $EDGESDIR/$e/obelisk-handoff;
done

# Show what's changed
git status -s
