CC=gcc
CFLAGS=-I/usr/lib/erlang/usr/include -fpic -shared
LDFLAGS=-lpthread -locci -lclntsh -lnnz11

all: compile

compile:
	$(CC) $(CFLAGS) $(LDFLAGS) netspire_oracle_drv.c -o ../../netspire-core/priv/lib/netspire_oracle_drv.so

clean:
	rm -f netspire_oracle.so
