.PHONY: all
.PRECIOUS: euler.o

all: problem1  problem2  problem3  problem4  problem5  problem6  problem7  \
     problem8  problem9  problem10 problem11 problem12 problem13 problem14 \
     problem15 problem16 problem17 problem18 problem19 problem20 problem21 \
     problem22 problem23 problem24 problem25 problem26 problem27 problem28 \
     problem29 problem30 problem31 problem32 problem33 problem34 problem35 \
     problem36 problem37 problem38 problem39 problem40 problem41 problem42 \
     problem43 problem44 problem45 problem46 problem47 problem48 problem49 \
     problem50

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

problem33: euler.mod euler.o problem33.o
	gfortran $@.o euler.o -o $@

problem35: euler.mod euler.o problem35.o
	gfortran $@.o euler.o -o $@

problem36: euler.mod euler.o problem36.o
	gfortran $@.o euler.o -o $@

problem37: euler.mod euler.o problem37.o
	gfortran $@.o euler.o -o $@

problem38: euler.mod euler.o problem38.o
	gfortran $@.o euler.o -o $@

problem40: euler.mod euler.o problem40.o
	gfortran $@.o euler.o -o $@

problem41: euler.mod euler.o problem41.o
	gfortran $@.o euler.o -o $@

problem46: euler.mod euler.o problem46.o
	gfortran $@.o euler.o -o $@

problem47: euler.mod euler.o problem47.o
	gfortran $@.o euler.o -o $@

problem49: euler.mod euler.o problem49.o
	gfortran $@.o euler.o -o $@

problem50: euler.mod euler.o problem50.o
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

cleanest: cleaner
