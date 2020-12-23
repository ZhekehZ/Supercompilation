
tests: 
    mkdir -p build
    ghc test/TestRunner.hs -i src/*.hs test/*Prog*.hs -odir build

clean: 
    rm -r build
