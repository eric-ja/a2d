

go: $(foreach go, $(shell find . -name go.sh), $(go)/run )

%/run: %
	cd $(abspath $</../..) && ./res/go.sh
