.PHONY : clean

out =
j-bin = /home/jrn/.guix-profile/bin
cflags = -Wall -shared -fpic -pthread
build : jpl-module.so

jpl-module.so : jpl-module.c
	gcc $(cflags) -L$(j-bin) -lj $< -o $@

install :
	mkdir -p $(out)
	cp jpl.so $(out)

clean :
	rm -rf *~ *.so *.o
