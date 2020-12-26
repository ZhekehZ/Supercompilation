all: tests clean

int:
	ghci test/TestRunner.hs -i src/*.hs test/*Prog*.hs

tests: 
	mkdir -p build
	ghc test/TestRunner.hs -i src/*.hs test/*Prog*.hs -outputdir build -O2 -o runTests && ./runTests

clean: 
	rm -rf build
	rm -rf runTests
