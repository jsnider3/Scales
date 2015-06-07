#!/usr/bin/env bash

scalac *.scala
scala scales.Main "tests/in/test1.uc"
jasmin *.j
java -verify Main
rm -f *.class *.j
scala scales.Main "tests/in/test2.uc"
jasmin *.j
java -verify Main
rm -f *.class *.j
#scala scales.Main "tests/in/test3.uc"
#rm -f *.class *.j
#scala scales.Main "tests/in/test4.uc"
#rm -f *.class *.j
#scala scales.Main "tests/in/test5.uc"
#rm -f *.class *.j
#scala scales.Main "tests/in/test6.uc"
#rm -f *.class *.j
#scala scales.Main "tests/in/test7.uc"
#rm -f *.class *.j
##scala scales.Main "tests/in/sort.uc"
#rm -f *.class *.j
