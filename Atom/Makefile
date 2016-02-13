clean:
	rm -f rm *.[cho] sonar *.hi

all: sonar sonar_example

sonar:
	ghc --make sonar.hs

sonar_example: sonar
	./sonar
	gcc -o sonar_example sonar_example.c
