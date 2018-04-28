
VERSION ?= 1.0.0f1-all

CC65 = ~/dev/cc65/bin
CAFLAGS = --target apple2enh --list-bytes 0 --relax-checks
CCFLAGS = --config asm.cfg


OUTDIR = out-$(VERSION)
OBJPATTERN = $(foreach outdir, $(OUTDIR), $(outdir)/%.o)

OBJS = out-%/dispatch.o \
       out-%/shifttables.o \
       out-%/hirestables.o \
       out-%/fillblit.o \
       out-%/rect.o \
       out-%/paintbits.o \
       out-%/polygons.o \
       out-%/fixeddiv.o \
       out-%/framepoly.o \
       out-%/lines.o \
       out-%/text.o \
       out-%/polybufs.o \
       out-%/graf.o \
       out-%/setzp.o \
       out-%/version.o \
       out-%/stdport.o \
       out-%/zpsave.o \
       out-%/cursor.o \
       out-%/start.o \
       out-%/events.o \
       out-%/menus.o \
       out-%/windows.o \
       out-%/drawicon.o \
       out-%/controls.o \
       out-%/keyboardmouse.o \
       out-%/setmenuselection.o \
       out-%/mouse.o \
       out-%/zp.o

PASCAL_OBJS = out-%/pascaldriver.o
  

all: out-$(VERSION)/mgtk out-$(VERSION)/mgtk.lib
.SECONDARY:

out-%/mgtk: out-%/mgtk.lib $(OBJS) empty.o
	$(CC65)/ld65 --config asm.cfg -m $@.map -o $@ --force-import MGTK__dispatch empty.o $<
	../res/listaddr.pl $@.map $(dir $@)/*.list

out-%/mgtk.pascal: out-%/mgtk.lib $(PASCAL_OBJS) $(OBJS)
	$(CC65)/ld65 --config asm-pascal.cfg -m $@.map -o $@ $(word 2, $^) $<

out-%/mgtk.lib: $(OBJS)
	-rm -f $@
	$(CC65)/ar65 a $@ $^

out-%/dump-exports: $(OBJS)
	$(CC65)/od65 --dump-exports $^ | egrep "Name: +\".*\"" | cut -d\" -f2

$(OUTDIR):
	mkdir -p $@

empty.o:
	$(CC65)/ca65 -o $@ /dev/null

%-exp.inc: %.s
	../res/genexports.pl MGTK < $< > $@

$(OBJPATTERN):  MGTK_VERSION_DEFS=$(shell ./res/parseversion.pl $(subst out-,,$(subst /,,$(dir $@))))
$(OBJPATTERN): %.s %-exp.inc $(OUTDIR)
	$(CC65)/ca65 $(CAFLAGS) -Din_mgtk=1 $(MGTK_VERSION_DEFS) --listing $(basename $@).list -o $@ $<
