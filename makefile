build-release:
	@dune runtest
	@dune build --profile release
	@cp _build/default/bin/main.exe otm