#!/usr/bin/env bash

scalac *.scala
echo "test1.uc"
scala scales.Main "tests/in/test1.uc"
jasmin *.j
java -verify Main > out.txt
[[ -z `diff out.txt tests/out/test1.out` ]] || exit
rm -f *.class *.j
echo "test2.uc"
scala scales.Main "tests/in/test2.uc"
jasmin *.j
java -verify Main > out.txt
[[ -z `diff out.txt tests/out/test2.out` ]] || exit
rm -f *.class *.j
echo "test3.uc"
scala scales.Main "tests/in/test3.uc"
jasmin *.j
java -verify Main > out.txt
[[ -z `diff out.txt tests/out/test3.out` ]] || exit
rm -f *.class *.j
echo "test4.uc"
scala scales.Main "tests/in/test4.uc"
jasmin *.j
rm -f *.class *.j
echo "test5.uc"
scala scales.Main "tests/in/test5.uc"
jasmin *.j
rm -f *.class *.j
echo "test6.uc"
scala scales.Main "tests/in/test6.uc"
jasmin *.j
java -verify Main > out.txt
[[ -z `diff out.txt tests/out/test6.out` ]] || exit
rm -f *.class *.j
echo "test7.uc"
scala scales.Main "tests/in/test7.uc"
rm -f *.class *.j
echo "sort.uc"
scala scales.Main "tests/in/sort.uc"
rm -f *.class *.j
echo "HOORAY!"
