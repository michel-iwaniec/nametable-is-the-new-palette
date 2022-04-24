ca65 main.s -v -g -l nametable_is_the_new_palette.lst
ld65 -o nametable_is_the_new_palette.nes -C config.cfg -v -m nametable_is_the_new_palette.map -vm --dbgfile nametable_is_the_new_palette.dbg main.o
