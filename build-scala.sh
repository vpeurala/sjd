#!/usr/bin/env bash
stack install
rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
stack exec sjd-gen test/data.txt
rc=$?; if [[ $rc != 0 ]]; then exit $rc; fi
cd src/main/java
javac -Xlint -Werror -cp ../../../lib/jackson-annotations-2.8.3.jar *.java
cd ../../..
