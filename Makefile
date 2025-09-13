CC = gcc
CFLAGS = -Wall -Wextra -O2
TARGET = myprog

$(TARGET): src/main.c
    $(CC) $(CFLAGS) -o $@ $^

clean:
    rm -f $(TARGET)
