.PHONY: all
.PRECIOUS: euler.o

all: problem1  problem2  problem3  problem4  problem5  problem6  problem7  \
     problem8  problem9  problem10 problem11 problem12 problem13 problem14 \
     problem15 problem16 problem17 problem18 problem19 problem20 problem21 \
     problem22 problem23 problem24 problem25 problem26 problem27 problem28 \
     problem29 problem30 problem31 problem32 problem33 problem34 problem35 \
     problem36 problem37 problem38 problem39 problem40 problem41 problem42 \
     problem43 problem44 problem45 problem46 problem47 problem48 problem49 \
     problem50 problem51 problem52 problem53 problem54 problem55 problem56 \
     problem57 problem58 problem59 problem60 problem61 problem62 problem63 \
     problem64 problem65 problem66 problem67 problem68 problem69 problem70 \
     problem71 problem72 problem73 problem74 problem75 problem76 problem77 \
     problem78 problem79

problem3: euler.mod euler.o problem3.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem4: euler.mod euler.o problem4.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem5: euler.mod euler.o problem5.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem7: euler.mod euler.o problem7.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem10: euler.mod euler.o problem10.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem12: euler.mod euler.o problem12.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem15: euler.mod euler.o problem15.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem21: euler.mod euler.o problem21.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem23: euler.mod euler.o problem23.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem27: euler.mod euler.o problem27.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem33: euler.mod euler.o problem33.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem35: euler.mod euler.o problem35.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem36: euler.mod euler.o problem36.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem37: euler.mod euler.o problem37.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem38: euler.mod euler.o problem38.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem40: euler.mod euler.o problem40.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem41: euler.mod euler.o problem41.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem46: euler.mod euler.o problem46.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem47: euler.mod euler.o problem47.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem49: euler.mod euler.o problem49.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem50: euler.mod euler.o problem50.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem51: euler.mod euler.o problem51.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem55: euler.mod euler.o problem55.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem56: problem56.f08
	gfortran -L$(HOME)/local/fmlib/8-20171224 -I$(HOME)/local/fmlib/8-20171224 $(CFLAGS) $@.f08 -lfm -o $@

problem57: problem57.f08
	gfortran -L$(HOME)/local/fmlib/8-20171224 -I$(HOME)/local/fmlib/8-20171224 $(CFLAGS) $@.f08 -lfm -o $@

problem58: euler.mod euler.o problem58.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem60: euler.mod euler.o problem60.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem61: euler.mod euler.o problem61.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem64: problem64.f08
	gfortran -L$(HOME)/local/fmlib/8-20171224 -I$(HOME)/local/fmlib/8-20171224 $(CFLAGS) $@.f08 -lfm -o $@

problem65: problem65.f08
	gfortran -L$(HOME)/local/fmlib/8-20171224 -I$(HOME)/local/fmlib/8-20171224 $(CFLAGS) $@.f08 -lfm -o $@

problem66: problem66.f08
	gfortran -L$(HOME)/local/fmlib/8-20171224 -I$(HOME)/local/fmlib/8-20171224 $(CFLAGS) $@.f08 -lfm -o $@

problem71: euler.mod euler.o problem71.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem73: euler.mod euler.o problem73.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem75: euler.mod euler.o problem75.f08
	gfortran $(CFLAGS) $@.f08 euler.o -o $@

problem78: problem78.f08
	gfortran -L$(HOME)/local/fmlib/8-20171224 -I$(HOME)/local/fmlib/8-20171224 $(CFLAGS) $@.f08 -lfm -o $@

%: %.f08
	gfortran $(CFLAGS) $< -o $@

euler.mod euler.o: euler.f08
	gfortran -O3 $< -c -o $@

clean:
	rm -f *.o *.mod

cleaner: clean
	rm -f problem? problem??

cleanest: cleaner
