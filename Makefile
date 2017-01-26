SRCS = modfnn.f90 main.f90

./a.out : $(SRCS) Makefile
	gfortran $(SRCS)
