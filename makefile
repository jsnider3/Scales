FILE='tests/in/test1.uc'
TEST='scala scales.Main ${FILE}'

all: scales
	@eval ${TEST}

scales:
	scalac *scala

clean:
	rm -rf *.class *.j scales
