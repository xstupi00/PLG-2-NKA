OUT=plg-2-nka
SRC=$(wildcard src/*.hs)
OBJ=$(wildcard src/*.o)
INT=$(wildcard src/*.hi)

all:
	ghc --make $(SRC) -o $(OUT)

clean:
	rm -f $(OUT) $(OBJ) $(INT) flp-fun-xstupi00.zip

pack:
	zip flp-fun-xstupi00.zip Makefile $(SRC) test/ README.md -r