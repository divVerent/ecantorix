#!/bin/sh

rm -rf espeak-data
cp -R /usr/share/espeak-data/ espeak-data
perl voiceshape.pl "$@" | tee espeak-data/voices/\!v/test
espeak --path=. -v en+test "The quick brown fox jumped over the lazy sleeping dog's back then sat on a tack."
