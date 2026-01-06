## VM Makefile
STACK=stack --stack-yaml stack.yaml
BINARY=glados-vm

all: build

build:
	$(STACK) build --copy-bins --local-bin-path ..

clean:
	$(STACK) clean

fclean: clean
	rm -rf .stack-work
	rm -f ../$(BINARY)

re: fclean all

.PHONY: all build clean fclean re

