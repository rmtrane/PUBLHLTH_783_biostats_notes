#!/bin/sh

set -e

[ -z "${GITHUB_PAT}" ] && exit 0
[ "${TRAVIS_BRANCH}" != "master" ] && exit 0

git config --global user.email "rmtrane@gmail.com"
git config --global user.name "rmtrane"

git clone -b gh-pages https://${GITHUB_PAT}@github.com/${TRAVIS_REPO_SLUG}.git book-output
cd book-output
cp -r ../_783_biostats_book/* ./
git add --all *
git commit -m "Update the book" || true
git push -q origin gh-pages
