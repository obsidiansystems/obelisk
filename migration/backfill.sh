#!/bin/sh -e

# Find hashes for each git commit, and create vertices list.
(for ref in `git log --abbrev-commit --pretty=oneline | awk '{print $1}'`; do migration/obelisk-upgrade.hash.sh . $ref; done) | uniq | perl -ne 'print if ++$k{$_}==1' > vertices

# Create edge pairs
cat vertices | python -c "import sys; inp=[s.strip() for s in sys.stdin.readlines()]; inp.reverse(); pairs=zip(inp, inp[1:]); print '\n'.join([a + '-' + b for (a,b) in pairs])" > edges

# Create the migration graph store
for e in `cat edges`; do mkdir -p migration/$e; touch migration/$e/obelisk-upgrade; touch migration/$e/obelisk-handoff; done

# Cleanup
rm -f vertices edges

# Show what's changed
git status -s
