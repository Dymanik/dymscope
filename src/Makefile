
all: 
	ghc Main.hs  -O2 --make -o ../dymScope -Wall


tests:
	 ghc --make -fhpc -o ../dymScope Main.hs -fforce-recomp
	 cd ..;ls -p examples | xargs -n1 -I{} bash -c 'echo {};./dymScope --dd examples/{}'
	 cd ..;ls -p examples | xargs -n1 -I{} bash -c 'echo {};./dymScope --ds examples/{}'
	 cd ..;ls -p examples | xargs -n1 -I{} bash -c 'echo {};./dymScope --sd examples/{}'
	 cd ..;ls -p examples | xargs -n1 -I{} bash -c 'echo {};./dymScope --ss examples/{}'

clean:
	rm ../dymScope *.hi *.o
