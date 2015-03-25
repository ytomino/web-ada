SOURCEDIR=../source

HOST=$(shell gcc -dumpmachine)
TARGET=$(HOST)

VERSION=$(shell gcc -dumpversion)

BUILDDIR=$(TARGET).build

ifeq ($(TARGET),$(HOST))
GNATPREFIX=
GCC=
BINLN=bin
else
GNATPREFIX=$(TARGET)-
GCC=$(TARGET)-gcc
BINLN=
endif

GFLAGS=
CFLAGS=-ggdb -pipe -gnatef -gnata -gnatwa -gnatyy-3chbs
MFLAGS=-D $(BUILDDIR)
BFLAGS=-E
LFLAGS=-ggdb

ifneq ($(DRAKE_RTSROOT),)
DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)/$(VERSION)
endif
ifneq ($(DRAKE_RTSDIR),)
GFLAGS+=--RTS=$(abspath $(DRAKE_RTSDIR))
endif

ifneq ($(GCC),)
GFLAGS+=$(filter -m32 -m64,$(GCC))
MFLAGS+=--GCC="$(GCC)"
LFLAGS+=--GCC="$(GCC)"
endif

EXAMPLES=$(basename $(filter-out b~%,$(wildcard *.adb)))

.PHONY: all clean $(EXAMPLES)

all: $(EXAMPLES)

$(BUILDDIR)/%: %.adb $(BUILDDIR) $(wildcard $(SOURCEDIR)/*)
	$(GNATPREFIX)gnatmake -c -I$(SOURCEDIR) $< $(GFLAGS) $(MFLAGS) -cargs $(CFLAGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatmake -b $(basename $(notdir $<)).ali $(GFLAGS) -bargs $(BFLAGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatlink -o ../$@ $(basename $(notdir $<)).ali $(GFLAGS) $(LFLAGS)

$(BUILDDIR):
	mkdir $(BUILDDIR)

$(BINLN): $(BUILDDIR)
	ln -s $(BUILDDIR) $(BINLN)

$(EXAMPLES): %: $(BUILDDIR)/% $(BINLN)

clean:
	-rm -rf *.build bin
