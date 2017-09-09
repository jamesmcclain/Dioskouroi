.PHONY: all
.PRECIOUS: euler.o

all: problem1  problem2  problem3  problem4  problem5  problem6  problem7  \
     problem8  problem9  problem10 problem11 problem12 problem13 problem14 \
     problem15 problem16 problem17 problem18 problem19 problem20 problem21 \
     problem22 problem23 problem24 problem25 problem26 problem27 problem28

problem3: euler.mod euler.o problem3.o
	gfortran $@.o euler.o -o $@

problem4: euler.mod euler.o problem4.o
	gfortran $@.o euler.o -o $@

problem5: euler.mod euler.o problem5.o
	gfortran $@.o euler.o -o $@

problem7: euler.mod euler.o problem7.o
	gfortran $@.o euler.o -o $@

problem10: euler.mod euler.o problem10.o
	gfortran $@.o euler.o -o $@

problem12: euler.mod euler.o problem12.o
	gfortran $@.o euler.o -o $@

problem15: euler.mod euler.o problem15.o
	gfortran $@.o euler.o -o $@

problem21: euler.mod euler.o problem21.o
	gfortran $@.o euler.o -o $@

problem23: euler.mod euler.o problem23.o
	gfortran $@.o euler.o -o $@

problem27: euler.mod euler.o problem27.o
	gfortran $@.o euler.o -o $@

%: %.o
	gfortran $< -o $@

euler.mod euler.o: euler.f08
	gfortran -O3 $< -c -o $@

%.o: %.f08
	gfortran -O3 $< -c -o $@

clean:
	rm -f *.o *.mod

cleaner: clean
	rm -f problem? problem??
