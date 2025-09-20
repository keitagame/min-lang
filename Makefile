CC = gcc
CFLAGS = -Wall -Wextra -O2

all: main

main: main.c
	$(CC) $(CFLAGS) -o main main.c

clean:
	rm -f $(TARGET)
