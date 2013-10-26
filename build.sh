#!/bin/sh

# Build script for elpa/marmalade uploads.

if [ ! -f elpy.el ]
then
    echo "Please run this from the elpy repository root." >&2
    exit 1
fi

VERSION=$(sed -ne 's/^;; Version: \(.*\)/\1/p' elpy.el)
BUILDDIR="dist/elpy-$VERSION"

rm -rf "$BUILDDIR"
mkdir -p "$BUILDDIR"

install -m 644 elpy-refactor.el LICENSE "$BUILDDIR/"
cat elpy.el \
| sed -e 's/(defconst elpy-version "devel"/(defconst elpy-version "'"$VERSION"'"/' \
> "$BUILDDIR/elpy.el"
install -m 644 README.rst "$BUILDDIR/README"
cp -r snippets "$BUILDDIR/"
cat elpy-pkg.el \
| sed -e 's/"devel"/"'"$VERSION"'"/g' \
> "$BUILDDIR/elpy-pkg.el"

tar -C "dist/" -c "elpy-$VERSION" > "dist/elpy-$VERSION.tar"

echo "dist/elpy-$VERSION.tar ready for upload."
