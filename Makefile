SRCS = modfnn.f03 main.f03

twolayer : modfnn.f03 twolayer.f03 Makefile
	gfortran -o twolayer modfnn.f03 twolayer.f03

./a.out : $(SRCS) Makefile
	gfortran $(SRCS)

clean:
	-rm a.out
