.PHONY: all
.PRECIOUS: euler.o

all: problem1  problem2  problem3  problem4  problem5  problem6  problem7 \
     problem8  problem9  problem10 problem11 problem12 problem13 problem14 \
     problem15 problem16 problem17 

%: %.o euler.mod euler.o
	gfortran $< euler.o -o $@

euler.mod euler.o: euler.f08
	gfortran -O3 $< -c -o $@

%.o: %.f08
	gfortran -O3 $< -c -o $@

clean:
	rm -f *.o *.mod

cleaner: clean
	rm -f problem? problem??
