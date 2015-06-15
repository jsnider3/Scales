#!/usr/bin/env bash

echo "test1.uc"
sbt -sbt-create run < "tests/in/test1.uc"
jasmin *.j
java -verify Main > out.txt
[[ -z `diff out.txt tests/out/test1.out` ]] || exit
rm -f *.class *.j
echo "test2.uc"
sbt -sbt-create run < "tests/in/test2.uc"
jasmin *.j
java -verify Main > out.txt
[[ -z `diff out.txt tests/out/test2.out` ]] || exit
rm -f *.class *.j
echo "test3.uc"
sbt -sbt-create run < "tests/in/test3.uc"
jasmin *.j
java -verify Main > out.txt
[[ -z `diff out.txt tests/out/test3.out` ]] || exit
rm -f *.class *.j
echo "test4.uc"
sbt -sbt-create run < "tests/in/test4.uc"
jasmin *.j
rm -f *.class *.j
echo "test5.uc"
sbt -sbt-create run < "tests/in/test5.uc"
jasmin *.j
rm -f *.class *.j
echo "test6.uc"
sbt -sbt-create run < "tests/in/test6.uc"
jasmin *.j
java -verify Main > out.txt
[[ -z `diff out.txt tests/out/test6.out` ]] || exit
rm -f *.class *.j
echo "test7.uc"
sbt -sbt-create run < "tests/in/test7.uc"
jasmin *.j
java -verify Main > out.txt
[[ -z `diff out.txt tests/out/test7.out` ]] || exit
rm -f *.class *.j
echo "sort.uc"
sbt -sbt-create run < "tests/in/sort.uc"
jasmin *.j
rm -f *.class *.j
echo "HOORAY!"
