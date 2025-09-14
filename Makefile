CC = gcc
CFLAGS = -Wall -Wextra -O2

all: main

main: main.o lexer.o
	$(CC) $(CFLAGS) -o main main.o lexer.o

main.o: src/main.c src/lexer.h
lexer.o: src/lexer.c src/lexer.h

clean:
	rm -f $(TARGET)
