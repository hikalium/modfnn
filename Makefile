SRCS = modfnn.f03 main.f03

./a.out : $(SRCS) Makefile
	gfortran $(SRCS)
