
CC65 = ~/dev/cc65/bin
CAFLAGS = --target apple2enh --list-bytes 0
CCFLAGS = --config asm-mgtk.cfg

OUTDIR = out

HEADERS = $(wildcard ../*.inc) $(wildcard ../inc/*.inc) $(wildcard *.inc)

DAS = calculator show.text.file date puzzle sort.directory \
	show.image.file this.apple eyes

TARGETS = $(patsubst %,$(OUTDIR)/%.$$F1,$(DAS))

MGTK_VERSION ?= 1.0.0f1
MGTK_DEFS = $(shell ../mgtk/res/parseversion.pl $(MGTK_VERSION))
MGTK_OBJS = $(shell make -sC ../mgtk out-$(MGTK_VERSION)/print-objs)


.PHONY: clean all
all: $(OUTDIR) $(TARGETS)

$(OUTDIR):
	mkdir -p $(OUTDIR)

clean:
	rm -f $(OUTDIR)/*.o
	rm -f $(OUTDIR)/*.list
	rm -f $(OUTDIR)/*.built
	rm -f $(OUTDIR)/*.\$$F1
	rm -f $(OUTDIR)/*.sys

$(OUTDIR)/%.o: %.s $(HEADERS)
	$(CC65)/ca65 $(CAFLAGS) $(MGTK_DEFS) --listing $(basename $@).list -o $@ $<

# Built DeskTop segments
$(OUTDIR)/%.built: $(OUTDIR)/%.o asm.cfg
	$(CC65)/ld65 $(CCFLAGS) -o '$@' $< $(MGTK_OBJS)

# Desk Accessories .$F1
$(OUTDIR)/%.$$F1: $(OUTDIR)/%.o
	$(CC65)/ld65 $(CCFLAGS) -o '$@' $< $(MGTK_OBJS)
	xattr -w prodos.AuxType '@' '$@'

# System Files .SYS
$(OUTDIR)/%.SYS: $(OUTDIR)/%.o
	$(CC65)/ld65 $(CCFLAGS) -o '$@' $<
	xattr -wx prodos.AuxType '00 20' $@
