.PHONY : clean

out =
j-bin = /home/jrn/.guix-profile/bin

build : jpl-module.so

jpl-module.so : jpl-module.c
	gcc -Wall -ggdb3 -shared -fpic -L$(j-bin) -lj $< -o $@

install :
	mkdir -p $(out)
	cp jpl.so $(out)

clean :
	rm -rf *~ *.so *.o
