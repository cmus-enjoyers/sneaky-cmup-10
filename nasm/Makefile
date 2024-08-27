NAME = main
LANG = asm
CC = nasm
CC_FLAGS = -f elf32
LD_FLAGS = -m elf_i386 -o $(NAME)

.PHONY: default build run clean
default: build run

build: $(NAME).$(LANG)
	@$(CC) ${CC_FLAGS} $(NAME).$(LANG)
	@ld $(LD_FLAGS) $(NAME).o
	@rm $(NAME).o

run: $(NAME)
	./$<

clean:
	rm -f $(NAME) $(NAME).o

