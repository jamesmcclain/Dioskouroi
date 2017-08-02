.PHONY: all
.PRECIOUS: %.o

all: problem1 problem2 problem3

%: %.o
	gfortran $< -o $@

%.o: %.f95
	gfortran -O3 $< -c -o $@

%.o: %.f08
	gfortran -O3 $< -c -o $@

clean:
	rm -f *.o

cleaner: clean
	rm -f problem?
