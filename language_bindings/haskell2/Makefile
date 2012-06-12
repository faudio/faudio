# Makefile is generated automatically: do not edit
# This Makefile builds a library for the package AUDIOENGINE

AR = /usr/bin/ar
AWK = /usr/bin/awk
MAKE = /usr/bin/make
GCC = /usr/bin/gcc -I../../include/
GHC = /usr/bin/ghc -I../../include/
EXTRA_OBJ = 
ECHO = /bin/echo
RM = /bin/rm
FIND = /usr/bin/find
GREP = /usr/bin/grep
MKDIR = /bin/mkdir
TOUCH = /usr/bin/touch
TRUE = /usr/bin/true
CP = /bin/cp
MV = /bin/mv
LD = /usr/bin/ld

define make-library
$(AWK) '\
BEGIN {\
  argbuf=""\
}\
{\
  argbuf=argbuf " " $$0 ;\
  if (length(argbuf) > 16384) {\
    system ("$(AR) qv $@ " argbuf) ;\
    argbuf=""\
  }\
}\
END {\
  system ("$(AR) qv $@ " argbuf)\
}\
'
endef

BASE = HS_$(PKG)_H

all:
	@$(ECHO) Please do not run make directly:
	@$(ECHO) Use runghc Setup.hs to build the library.

build : loc $(LOC)/libHS$(PKG)-$(VER).a $(LOC)/HS$(PKG)-$(VER).o
	$(CP) -p *.hi $(LOC)

loc:
	-$(MKDIR) -p $(LOC)

$(LOC)/HS$(PKG)-$(VER).o : $(LOC)/libHS$(PKG)-$(VER).a
	$(LD) -r --whole-archive -o $@ $<

$(LOC)/libHS$(PKG)-$(VER).a : $(BASE).hsc
	$(ECHO) "include Makefile" > $(BASE)_depend
	$(GHC) -package HSFFIG -M $(BASE)*.hs -dep-makefile $(BASE)_depend
	$(GHC) -c $(BASE)_hsc.c
	$(MAKE) -f $(BASE)_depend $(BASE)_S_t.o $(BASE)_S_n.o $(BASE)_S_d.o \
		$(BASE)_C.o $(BASE)_S.o $(BASE).o
	$(RM) -f $@
	$(AR) cq $@ $(BASE)_hsc.o $(EXTRA_OBJ)
	$(FIND) . -name '$(BASE)*.o' | $(GREP) -E 'split/|stub' | $(make-library)


%.hi %.o : %.hs
	-$(MKDIR) $*_split 2>/dev/null
	$(ECHO) "int dummy_$*_stub_entry;" | $(GCC) -x c -c -o $*.o -
	$(GHC) -package-name $(PKG)-$(VER) -split-objs -c -package HSFFIG $<
	$(TOUCH) $*.o $*.hi

%.hi : %.o
	$(TRUE)


