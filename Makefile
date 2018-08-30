SRC = stack.yaml tt.cabal $(wildcard src/*.hs)
TT = ~/.local/bin/tt

all: $(TT)

clean:
	stack clean
	rm $(TT)

$(TT): $(SRC)
	stack build
	stack install
