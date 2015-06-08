FILE='tests/in/test1.uc'
TEST='scala scales.Main ${FILE}'

all:
	scalac *scala
	@eval ${TEST}

clean:
	rm -rf *.class *.j scales
