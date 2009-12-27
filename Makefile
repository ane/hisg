GHC = ghc

MAIN = hessu

all: $(MAIN)

$(MAIN):
	$(GHC) -O2 --make Main -o $(MAIN)

clean:
	del *.o
	del *.hi
