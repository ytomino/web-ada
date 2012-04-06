SOURCEDIR=../source

HOST=$(shell gcc -dumpmachine)
TARGET=$(HOST)

ifeq ($(TARGET),$(HOST))
GNATMAKE=gnatmake
BUILDDIR=build
else
GNATMAKE=$(TARGET)-gnatmake
BUILDDIR=build-$(TARGET)
endif

ifneq ($(DRAKE_RTSROOT),)
DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)
endif
ifneq ($(DRAKE_RTSDIR),)
MFLAGS=--RTS=$(abspath $(DRAKE_RTSDIR))
endif

CFLAGS=-gnata -gnatwa -gnatyy-3chbs

.PHONY: all clean

all: $(BUILDDIR)/test_encode $(BUILDDIR)/test_time $(BUILDDIR)/test_web

$(BUILDDIR)/%: %.adb $(BUILDDIR) $(IMPORTDIR) $(wildcard $(SOURCEDIR)/*)
	cd $(BUILDDIR) && $(GNATMAKE) -g $(MFLAGS) -I../$(SOURCEDIR) -I../$(IMPORTDIR) $(CFLAGS) ../$< -largs $(LFLAGS)

$(BUILDDIR):
	mkdir $(BUILDDIR)

clean:
	-rm -rf build*
