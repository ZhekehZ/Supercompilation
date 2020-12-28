IntProgram = ../app/IntProgram.hs
LIB = ../lib/*.hs
BUILD_DIR = build

all: test

test: 
	mkdir -p ${BUILD_DIR}
	ghc TestRunner.hs -i ${LIB} ${IntProgram} Program*.hs -O2 -outputdir ${BUILD_DIR}  -o testRunner && ./testRunner

clean: 
	rm -rf ${BUILD_DIR}
	rm -rf testRunner
