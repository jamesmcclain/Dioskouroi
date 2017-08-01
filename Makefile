.PHONY: all
.PRECIOUS: %.o

all: problem1

%: %.o
	gfortran $< -static-libgfortran -o $@

%.o: %.f95
	gfortran -O3 $< -c -o $@

clean:
	rm -f *.o

cleaner: clean
	rm -f problem?
