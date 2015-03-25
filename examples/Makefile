SOURCEDIR=../source

HOST=$(shell gcc -dumpmachine)
TARGET=$(HOST)

VERSION=$(shell gcc -dumpversion)

BUILDDIR=$(TARGET).build

ifeq ($(TARGET),$(HOST))
GNATMAKE=gnatmake
BINLN=bin
else
GNATMAKE=$(TARGET)-gnatmake
BINLN=
endif

ifneq ($(DRAKE_RTSROOT),)
DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)/$(VERSION)
endif
ifneq ($(DRAKE_RTSDIR),)
MFLAGS=--RTS=$(abspath $(DRAKE_RTSDIR))
endif

EXAMPLES=$(basename $(filter-out b~%,$(wildcard *.adb)))

CFLAGS=-gnata -gnatwa -gnatyy-3chbs

.PHONY: all clean $(EXAMPLES)

all: $(BUILDDIR)/test_encode $(BUILDDIR)/test_time $(BUILDDIR)/test_web $(BINLN)

$(BUILDDIR)/%: %.adb $(BUILDDIR) $(IMPORTDIR) $(wildcard $(SOURCEDIR)/*)
	cd $(BUILDDIR) && $(GNATMAKE) -g $(MFLAGS) -I../$(SOURCEDIR) $(CFLAGS) ../$< -largs $(LFLAGS)

$(BINLN): $(BUILDDIR)
	ln -s $(BUILDDIR) $(BINLN)

$(EXAMPLES): %: $(BUILDDIR)/%

$(BUILDDIR):
	mkdir $(BUILDDIR)

clean:
	-rm -rf *.build bin
