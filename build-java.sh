#!/usr/bin/env bash
stack install
stack exec sjd-gen test/customer.sjd
cd src/main/java
javac -Xlint -Werror -cp ../../../lib/jackson-annotations-2.8.3.jar *.java
cd ../../..
