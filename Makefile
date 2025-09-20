CC = gcc
CFLAGS = -Wall -Wextra -O2

all: main

main: main.o lexer.o
	$(CC) $(CFLAGS) -o main main.c

clean:
	rm -f $(TARGET)
