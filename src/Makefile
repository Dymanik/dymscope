flags=-O2 --make -Wall -fno-warn-unused-do-bind

all: main gui

main:
	ghc Main.hs  -o ../dymScope  ${flags} 

gui:
	ghc gui.hs -o ../dymScopeGtk ${flags}

tests:
	 ghc --make -fhpc -o ../dymScope Main.hs -fforce-recomp
	 ghc --make -fhpc -o ../dymScopeGtk gui.hs -fforce-recomp
	 cd ..;ls -p examples | xargs -n1 -I{} bash -c 'echo {};./dymScope --dd examples/{}'
	 cd ..;ls -p examples | xargs -n1 -I{} bash -c 'echo {};./dymScope --ds examples/{}'
	 cd ..;ls -p examples | xargs -n1 -I{} bash -c 'echo {};./dymScope --sd examples/{}'
	 cd ..;ls -p examples | xargs -n1 -I{} bash -c 'echo {};./dymScope --ss examples/{}'

clean:
	rm ../dymScope *.hi *.o
