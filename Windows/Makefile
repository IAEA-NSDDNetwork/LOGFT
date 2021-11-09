libnndc.a: nndclib.f
	gfortran -c nndclib.f
	ar cr libnndc.a nndclib.o

logft.exe: libnndc.a logft.f
	gfortran -static logft.f  -L. -lnndc -o logft.exe
clean:
	rm -f *.o
	rm -f *.a
	rm -f logft
