.PHONY: all

all: problem1

%: %.f95
	gfortran $< -ggdb3 -o $@

