all: tests clean

int:
	ghci test/TestRunner.hs -i src/*.hs test/*Prog*.hs test/Subst*

tests: 
	mkdir -p build
	ghc test/TestRunner.hs -i src/*.hs test/*Prog*.hs -outputdir build -o runTests && ./runTests

clean: 
	rm -rf build
	rm -rf runTests
