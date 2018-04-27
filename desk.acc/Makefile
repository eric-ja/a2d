
CC65 = ~/dev/cc65/bin
CAFLAGS = --target apple2enh --list-bytes 0
CCFLAGS = --config asm-mgtk.cfg

OUTDIR = out

HEADERS = $(wildcard ../*.inc) $(wildcard ../inc/*.inc) $(wildcard *.inc)

DAS = $(shell cat TARGETS)

TARGETS = $(patsubst %,$(OUTDIR)/%.built,$(DAS))

MGTK_VERSION ?= 1.0.0f1-all
MGTK_DEFS = $(shell ../mgtk/res/parseversion.pl $(MGTK_VERSION))
MGTK_LIB = ../mgtk/out-$(MGTK_VERSION)/mgtk.lib


.PHONY: clean all
all: $(OUTDIR) $(TARGETS)

$(OUTDIR):
	mkdir -p $(OUTDIR)

$(MGTK_LIB):
	make -C ../mgtk out-$(MGTK_VERSION)/mgtk.lib

clean:
	rm -f $(OUTDIR)/*.o
	rm -f $(OUTDIR)/*.list
	rm -f $(OUTDIR)/*.built
	rm -f $(OUTDIR)/*.sys

$(OUTDIR)/%.o: %.s $(HEADERS)
	$(CC65)/ca65 $(CAFLAGS) $(MGTK_DEFS) --listing $(basename $@).list -o $@ $<

# Built DeskTop segments
$(OUTDIR)/%.built: $(OUTDIR)/%.o asm.cfg $(MGTK_LIB)
	$(CC65)/ld65 $(CCFLAGS) -o '$@' $< $(MGTK_LIB)

# Desk Accessory Files
$(OUTDIR)/%.built: $(OUTDIR)/%.o $(MGTK_LIB)
	$(CC65)/ld65 $(CCFLAGS) -o '$@' $< $(MGTK_LIB)

# System Files .SYS
$(OUTDIR)/%.SYS: $(OUTDIR)/%.o
	$(CC65)/ld65 $(CCFLAGS) -o '$@' $<
	xattr -wx prodos.AuxType '00 20' $@
