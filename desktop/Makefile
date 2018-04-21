
CC65 = ~/dev/cc65/bin
CAFLAGS = --target apple2enh --list-bytes 0
CCFLAGS = --config asm.cfg
CCFLAGS_MGTK = --config asm-mgtk.cfg

OUTDIR = out

HEADERS = $(wildcard ../*.inc) $(wildcard ../inc/*.inc) $(wildcard *.inc)

SEGMENTS = loader mgtk desktop invoker \
        ovl1 ovl1a ovl1b ovl1c ovl2 ovl34567

TARGETS = $(patsubst %,$(OUTDIR)/%.built,$(SEGMENTS)) out/sys.SYS

MGTK_VERSION = 1.0.0f1
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
	rm -f $(OUTDIR)/*.\$$F1
	rm -f $(OUTDIR)/*.sys

$(OUTDIR)/ovl34567.o: ovl34567.s $(HEADERS) ovl3.s ovl4.s ovl5.s ovl6.s ovl7.s
	$(CC65)/ca65 $(CAFLAGS) $(MGTK_DEFS) --listing $(basename $@).list -o $@ $<

$(OUTDIR)/%.o: %.s $(HEADERS)
	$(CC65)/ca65 $(CAFLAGS) $(MGTK_DEFS) --listing $(basename $@).list -o $@ $<

$(OUTDIR)/mgtk.built: $(MGTK_LIB) asm.cfg
	$(CC65)/ld65 $(CCFLAGS_MGTK) -o '$@' $(MGTK_LIB)

# Built DeskTop segments
$(OUTDIR)/%.built: $(OUTDIR)/%.o $(MGTK_LIB) asm.cfg
	$(CC65)/ld65 $(CCFLAGS_MGTK) -o '$@' $< $(MGTK_LIB)

# Desk Accessories .$F1
$(OUTDIR)/%.$$F1: $(OUTDIR)/%.o
	$(CC65)/ld65 $(CCFLAGS_MGTK) -o '$@' $<
	xattr -w prodos.AuxType '@' '$@'

# System Files .SYS
$(OUTDIR)/%.SYS: $(OUTDIR)/%.o
	$(CC65)/ld65 $(CCFLAGS) -o '$@' $<
	xattr -w prodos.AuxType '00 20' $@
