build-release:
	@dune runtest
	@dune build --profile release
	@rm -f otm
	@cp _build/default/bin/main.exe otm