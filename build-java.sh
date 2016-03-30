#!/usr/bin/env bash
stack install
stack exec sjd-gen test/customer.sjd
cd src/main/java
javac -cp /Users/vpeurala/.m2/repository/com/fasterxml/jackson/core/jackson-annotations/2.8.0-SNAPSHOT/jackson-annotations-2.8.0-SNAPSHOT.jar *.java
cd ../../..

