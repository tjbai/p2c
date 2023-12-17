CC=gcc
CFLAGS=-I.

all: run

run: sample1.o sample2.o
	$(CC) -o run sample1.o sample2.o

sample1.o: sample1.c sample1.h
	$(CC) -c sample1.c $(CFLAGS)

sample2.o: sample2.c sample2.h
	$(CC) -c sample2.c $(CFLAGS)

clean:
	rm -f *.o run