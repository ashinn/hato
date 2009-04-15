
CSC=csc
CHICKEN_OPTS=-O2 -G

SOURCES=hato-prob.scm hato-mime.scm hato-archive.scm hato-smtp.scm \
	hato-base64.scm quoted-printable.scm domain-keys.scm \
	html-parser.scm hato-i3db.scm hato-uri.scm user-env.scm dns.scm \
	hato-spf.scm hato-pop.scm hato-imap.scm hato-daemon.scm \
	hato-db.scm hato-config.scm hato-rfc3028.scm hato-date.scm \
	hato-nntp.scm hato-filter.scm lru-cache.scm
MODULES=$(SOURCES:%.scm=%.so)

INSTALL	= /usr/bin/install -c
RM          = rm
PREFIX      = /usr/local
EXEC_PREFIX = ${PREFIX}
BIN_DIR     = ${EXEC_PREFIX}/bin
LIB_DIR     = ${EXEC_PREFIX}/lib
SRCDIR      = .
DATADIR     = ${PREFIX}/share

########################################################################

.PHONY: all modules doc dist clean cleaner test install uninstall

all: modules hato-mta hato-classify hato-fetch

modules: $(MODULES)

%.so: %.scm
	$(CSC) $(CHICKEN_OPTS) -s -emit-exports $(<:%.scm=%.exports) $< \
	    2>&1 | ./filter-csc-warnings.scm

########################################################################

hato-mta: hato-mta.scm let-args.scm
	$(CSC) $(CHICKEN_OPTS) -o $@ $< 2>&1 | ./filter-csc-warnings.scm

hato-classify: hato-classify.scm let-args.scm
	$(CSC) $(CHICKEN_OPTS) -o $@ $< 2>&1 | ./filter-csc-warnings.scm

hato-classify-static: hato-classify.scm let-args.scm $(SOURCES)
	$(CSC) $(CHICKEN_OPTS) -unsafe -static -feature static -o $@ $<

hato-fetch: hato-fetch.scm
	$(CSC) $(CHICKEN_OPTS) -o $@ $< 2>&1 | ./filter-csc-warnings.scm

run-compare: run-compare.scm
	$(CSC) $(CHICKEN_OPTS) -o $@ $<

test-classify: test-classify.scm
	$(CSC) $(CHICKEN_OPTS) -o $@ $<

########################################################################

doc:
	tex2page hato-manual.tex

dist:
	find . -name \*~ -exec rm -f '{}' \;
	mkdir hato-`cat VERSION`
	cd hato-`cat VERSION`; for f in `find ../_darcs/current -maxdepth 1`; do ln -s $$f; done; cd ..
	cd hato-`cat VERSION`; for f in `echo ../*.html ../*.css`; do ln -s $$f; done; cd ..
	tar cphzvf hato-`cat VERSION`.tar.gz hato-`cat VERSION`
	rm -rf hato-`cat VERSION`

install: all
	$(INSTALL) hato-mta $(BIN_DIR)
	$(INSTALL) hato-classify $(BIN_DIR)
	$(INSTALL) hato-fetch $(BIN_DIR)
	$(INSTALL) $(MODULES) $(MODULES:%.so=%.exports) \
	    `chicken-setup -repository`

uninstall:
	$(RM) -f $(BIN_DIR)/hato-mta
	$(RM) -f $(BIN_DIR)/hato-classify
	$(RM) -f $(BIN_DIR)/hato-fetch
	cd `chicken-setup -repository` && \
	    $(RM) -f $(MODULES) $(MODULES:%.so=%.exports)

clean:
	rm -f *~ */*~ */*/*~ *.so *.exports hato-mta hato-classify hato-classify-static hato-fetch run-compare

cleaner: clean
	rm -rf tests/roots/*/root/queue
	rm -rf tests/roots/*/root/mail
	rm -rf tests/roots/*/root/run

test:
	cd tests && csi -I .. -script mta-test.scm

