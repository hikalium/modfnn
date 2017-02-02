default:
	make mnist
	make twolayer

mnist : modfnn.f03 mnist.f03 Makefile
	gfortran -o mnist modfnn.f03 mnist.f03

twolayer : modfnn.f03 twolayer.f03 Makefile
	gfortran -o twolayer modfnn.f03 twolayer.f03

clean:
	-rm twolayer
	-rm mnist
