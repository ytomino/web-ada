SOURCEDIR=../source

HOST:=$(shell gcc -dumpmachine)
TARGET=$(HOST)

BUILDDIR=$(TARGET).noindex

ifneq ($(TARGET),$(HOST))
 GCCPREFIX=$(TARGET)-
 BINLN=
else
 GCCPREFIX=
 BINLN=bin
endif
GCCSUFFIX=
GCC=$(GCCPREFIX)gcc$(GCCSUFFIX)

CFLAGS=-ggdb -pipe
CFLAGS_ADA=-gnata -gnatef -gnatwa -gnatyy-3bchms
LDFLAGS=-ggdb

ifneq ($(DRAKE_RTSROOT),)
 VERSION:=$(shell $(GCC) -dumpversion)
 DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)/$(VERSION)
 ifneq ($(wildcard $(DRAKE_RTSDIR)/debug),)
  DRAKE_RTSDIR:=$(DRAKE_RTSDIR)/debug
 endif
endif

GARGS=$(addprefix --RTS=,$(abspath $(DRAKE_RTSDIR)))
MARGS=-D $(BUILDDIR) -gnatA $(addprefix -gnatec=,$(wildcard *.adc)) \
	-I$(SOURCEDIR)
CARGS=$(CFLAGS) $(CFLAGS_ADA)
BARGS=-E -x
LARGS=$(LDFLAGS)

ifneq ($(GCC),gcc)
 GARGS+=$(filter -m32 -m64,$(GCC))
 MARGS+=--GCC="$(GCC)"
 LARGS+=--GCC="$(GCC)"
endif

EXAMPLES=$(basename $(filter-out b~%,$(wildcard *.adb)))
TESTS=$(filter test_%,$(EXAMPLES))

.PHONY: all check clean $(EXAMPLES)

all: $(addprefix $(BUILDDIR)/,$(EXAMPLES)) $(BINLN)

$(BUILDDIR)/%: %.adb $(wildcard $(SOURCEDIR)/*) | $(BUILDDIR)
	$(GCCPREFIX)gnatmake$(GCCSUFFIX) -c $< $(GARGS) $(MARGS) -cargs $(CARGS)
	cd $(BUILDDIR) && $(GCCPREFIX)gnatbind$(GCCSUFFIX) \
		$(basename $(notdir $<)).ali $(GARGS) $(BARGS)
	cd $(BUILDDIR) && $(GCCPREFIX)gnatlink$(GCCSUFFIX) -o ../$@ \
		$(basename $(notdir $<)).ali $(GARGS) $(LARGS)

$(BUILDDIR):
	mkdir $(BUILDDIR)

$(BINLN): | $(BUILDDIR)
	ln -s $(BUILDDIR) $(BINLN)

$(filter-out test_%,$(EXAMPLES)): %: $(BUILDDIR)/% $(BINLN)

check: all $(TESTS)

$(TESTS): %: $(BUILDDIR)/%
	$<

clean:
	-[ -h "$(BINLN)" ] && rm "$(BINLN)"
	-rm -r $(BUILDDIR)
