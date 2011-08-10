SOURCEDIR=../source
BUILDDIR=build

ifneq ($(DRAKE_RTSROOT),)
HOST=$(shell gcc -dumpmachine)
TARGET=$(HOST)
DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)
endif
ifneq ($(DRAKE_RTSDIR),)
MFLAGS=--RTS=$(abspath $(DRAKE_RTSDIR))
endif

CFLAGS=-gnata -gnatwa -gnatyy-3chbs

.PHONY: all clean $(BUILDDIR)/test_web $(BUILDDIR)/test_encode

all: $(BUILDDIR)/test_web $(BUILDDIR)/test_encode

$(BUILDDIR)/test_web: test_web.adb $(BUILDDIR)
	cd $(BUILDDIR) && gnatmake -g $(MFLAGS) -I../$(SOURCEDIR) $(CFLAGS) ../$< $(LFLAGS)

$(BUILDDIR)/test_encode: test_encode.adb $(BUILDDIR)
	cd $(BUILDDIR) && gnatmake -g $(MFLAGS) -I../$(SOURCEDIR) $(CFLAGS) ../$< $(LFLAGS)

$(BUILDDIR):
	mkdir $(BUILDDIR)

clean:
	-rm -rf build
