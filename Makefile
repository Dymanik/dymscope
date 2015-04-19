

all: dymscope

dymscope: 
	cd src ;make

tests:
	rm *.tix || cd src; make tests


clean:
	cd src; make clean
