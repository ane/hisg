GHC = ghc

MAIN = hisg

all:
	$(GHC) -O2 --make Hisg -o $(MAIN)

clean:
	rm *.o Hisg/*.o
	rm *.hi Hisg/*.hi
