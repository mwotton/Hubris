all: libdynhs.so
libdynhs.so: Test.hs
	../bin/jhc_builder Test.hs

clean:
	rm -rf tmp/ tmp.old hs.out libdynhs.so; true
