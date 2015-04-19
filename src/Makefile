flags=-O2 --make -Wall -fno-warn-unused-do-bind

all: main gui

main:
	ghc Main.hs  -o ../dymScope  ${flags} 

gui:
	ghc gui.hs -o ../dymScopeGtk ${flags}

tests:
	 ghc --make -fhpc -o ../dymScope Main.hs -fforce-recomp
	 ghc --make -fhpc -o ../dymScopeGtk gui.hs -fforce-recomp
	 cd ..; find examples -name *.ds | xargs -n1 -I{} bash -c 'echo {};./dymScope --dd {}'
	 cd ..; find examples -name *.ds | xargs -n1 -I{} bash -c 'echo {};./dymScope --ds {}'
	 cd ..; find examples -name *.ds | xargs -n1 -I{} bash -c 'echo {};./dymScope --sd {}'
	 cd ..; find examples -name *.ds | xargs -n1 -I{} bash -c 'echo {};./dymScope --ss {}'

clean:
	rm ../dymScope *.hi *.o
