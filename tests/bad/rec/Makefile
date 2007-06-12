# -*- Makefile -*-
# $Id: Makefile.test 263 2005-11-08 16:24:18Z regisgia $
.PHONY: check bad good clean all clean-silent annnounce expected

OPTIONS=$(shell if test -f options; then cat options; else echo ""; fi)
MINI=../../../src/pfiou -e $(OPTIONS)
BAD_INPUTS=$(wildcard *.bad-input)
BAD_OUTPUTS=$(BAD_INPUTS:.bad-input=.bad-output)
GOOD_INPUTS=$(wildcard *.good-input)
GOOD_OUTPUTS=$(GOOD_INPUTS:.good-input=.good-output)
GOOD_EXPECTED=$(GOOD_INPUTS:.good-input=.expected)

all:

clean-expected:
	@ rm --force *.expected

expected: clean-expected $(GOOD_EXPECTED)

%.expected: %.good-input
	@ $(MINI) $< &> $@ || echo "$@ has failed."; touch $@

%.bad-output:%.bad-input						
	@ echo TEST >> bad-counter;					 \
	($(MINI) $< &> $@ && echo "[ KO ]   --> " $<			 \
	" should have failed !")					 \
	 || (echo "[XFAIL] " $<; echo 'XF' >> bad-ok-counter);		 \
	 if [ x$$VERBOSE == x1 ]; then					 \
	echo;								 \
	echo "INPUT: ";							 \
	cat $<;								 \
	echo;								 \
	echo "OUTPUT: ";						 \
	cat $@;								 \
	echo "--------------------------------------------------------"; \
	fi

%.good-output:%.good-input						
	@ (echo TEST >> good-counter); $(MINI) $< &> $@;                 \
	(diff -u $@ $*.expected &> /dev/null				 \
	  && (echo "[ OK ]  " $<; (echo "OK $@" >> good-ok-counter)))	 \
	  || (echo "[ KO ]   --> " $<); if [ x$$VERBOSE == x1 ]; then	 \
	echo;								 \
	echo "INPUT:";							 \
	cat $<;								 \
	echo;								 \
	echo "OUTPUT: ";						 \
	cat $@;								 \
	echo "EXPECTED: ";						 \
	cat $*.expected;						 \
	echo "--------------------------------------------------------"; \
	fi

clean-bad-counters:
	@ rm --force bad-ok-counter bad-counter

clean-good-counters:
	@ rm --force good-counter good-ok-counter

bad: clean-bad-counters $(BAD_OUTPUTS)
	@ touch bad-ok-counter
	@ touch bad-counter
	@ echo "BAD:  "`wc -l bad-ok-counter | cut -f1 -d' '` \
	/ `wc -l bad-counter | cut -f1 -d' '`

good: clean-good-counters $(GOOD_OUTPUTS) 
	@ touch good-ok-counter
	@ touch good-counter
	@ echo "GOOD: "`wc -l good-ok-counter | cut -f1 -d' '` \
	/ `wc -l good-counter | cut -f1 -d' '` 

announce:
	@ echo "========================================================"; 
	@ echo "In "`pwd | sed 's/.*\/\(.*\/.*\)/\1/' `

check: announce clean-silent good bad

clean:
	$(RM) --force *.good-output *.bad-output *counter *~

clean-silent:
	@ $(RM) --force *.good-output *.bad-output *counter *~
