BUILD_DIR = build
LIB = lib/*.hs

all: tests sc

repl:
	ghci -i ${LIB} app/*.hs test/Program*.hs

sc:
	mkdir -p ${BUILD_DIR}
	ghc app/SuperCompiler.hs -i ${LIB} app/Parser.hs app/IntProgram.hs -outputdir ${BUILD_DIR} -O2 -o sc

tests: 
	$(MAKE) -C test

clean: 
	$(MAKE) -C test clean
	rm -rf ${BUILD_DIR}
	rm -rf sc
