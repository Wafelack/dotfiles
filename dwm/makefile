CC := gcc
OUT := dwm_status
CFLAGS := 	\
	-Wall			\
	-Wextra		\
	-Werror		\
	-ansi			\
	-pedantic

all:
	$(CC) $(CFLAGS) -o $(OUT) status.c
install: CFLAGS+=-O3
install: all
	cp $(OUT) /usr/bin
