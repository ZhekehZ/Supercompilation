BUILD_DIR = build
LIB = src/*.hs
TEST_LIB = test/P*.hs
PARSER_LIB = parser/*.hs
TEST_RUNNER = test/TestRunner.hs
APP = SuperCompiler.hs

all: tests app

repl:
	ghci -i ${LIB} ${TEST_LIB}

tests: 
	mkdir -p build
	ghc ${TEST_RUNNER} -i ${LIB} ${TEST_LIB} -outputdir ${BUILD_DIR} -O2 -o runTests && ./runTests

app: 
	mkdir -p build
	ghc ${APP} -i ${LIB} ${TEST_LIB} ${PARSER_LIB} -outputdir ${BUILD_DIR} -O2 -o sc

clean: 
	rm -rf ${BUILD_DIR}
	rm -rf runTests
	rm -rf sc
