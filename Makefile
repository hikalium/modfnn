twolayer : modfnn.f03 twolayer.f03 Makefile
	gfortran -o twolayer modfnn.f03 twolayer.f03

clean:
	-rm twolayer
