.PHONY : clean

out =
j-bin = /home/jrn/.guix-profile/bin
cflags = -g -Wall -shared -fpic -lpthread
build : jpl-module.so

jpl-module.so : jpl-module.c
	gcc $(cflags) -L$(j-bin) -lj $< -o $@

install :
	mkdir -p $(out)
	cp jpl-module.so $(out)

clean :
	rm -rf *~ *.so *.o
