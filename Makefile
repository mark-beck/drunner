build: bin/main.ml bin/dune
	@dune build

	@echo "Build successful"

.Phony: clean
clean:
	@dune clean
	@echo "Clean successful"

.Phony: install
install:
	@cp _build/install/default/bin/drunner /usr/bin/drunner
	@echo "Install successful"