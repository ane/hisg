GHC = ghc

MAIN = hisg

all:
	$(GHC) -O2 -threaded --make Hisg -o $(MAIN)

prof:
	$(GHC) -O2 -rtsopts -threaded -prof -auto-all -caf-all -fforce-recomp --make Hisg -o $(MAIN)

clean:
	rm *.o Hisg/*.o
	rm *.hi Hisg/*.hi
