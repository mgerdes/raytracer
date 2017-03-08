all:
	ghc -O2 --make Main

clean:
	rm *.o *.hi Main
