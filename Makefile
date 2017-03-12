all:
	ghc -O2 --make -Wall Main

clean:
	rm *.o *.hi Main
