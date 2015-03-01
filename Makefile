

all: dymscope

dymscope: 
	cd src ;make

tests:
	rm dymScope.tix || cd src; make tests


clean:
	cd src; make clean
