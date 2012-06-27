SUBDIRS = libs engines

install:
	-for d in $(SUBDIRS); do (cd $$d; make install); done

clean:
	-for d in $(SUBDIRS); do (cd $$d; make clean); done

build:
	-for d in $(SUBDIRS); do (cd $$d; make build); done
