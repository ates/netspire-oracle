ERL=erl
EFLAGS=+W w

all: compile

compile:
	(cd c_src; make compile)
	test -d ebin || mkdir ebin
	$(ERL) $(EFLAGS) -make

test:
	erl -pa ebin

clean:
	make -C c_src clean
	rm -rf ebin
