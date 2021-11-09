libnndc.a: nndclib.f
	gfortran -c nndclib.f
	ar cr libnndc.a nndclib.o

logft: libnndc.a logft.f
	gfortran -o logft -L. -lnndc logft.f

clean:
	rm *.o
	rm *.a
	
