CC = gcc
CFLAGS = -Wall -Wextra -O2

all: main

main: main.o lexer.o
	$(CC) $(CFLAGS) -o main main.o lexer.o

main.o: main.c lexer.h
lexer.o: lexer.c lexer.h

clean:
	rm -f $(TARGET)
