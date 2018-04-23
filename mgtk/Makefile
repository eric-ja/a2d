
VERSION ?= 1.0.0f1

CC65 = /distfiles/cc65-master/bin/
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
  

all: out-$(VERSION)/mgtk out-$(VERSION)/mgtk.lib
.SECONDARY:

out-%/mgtk: $(OBJS)
	$(CC65)/ld65 --config asmo65.cfg -m $@.map -o $@ $^

out-%/mgtk.lib: $(OBJS)
	-rm $@
	$(CC65)/ar65 a $@ $^

$(OUTDIR):
	mkdir -p $@

%-exp.inc: %.s
	../res/genexports.pl MGTK < $< > $@

$(OBJPATTERN):  MGTK_VERSION_DEFS=$(shell ./res/parseversion.pl $(subst out-,,$(subst /,,$(dir $@))))
$(OBJPATTERN): %.s %-exp.inc $(OUTDIR)
	$(CC65)/ca65 $(CAFLAGS) $(MGTK_VERSION_DEFS) --listing $(basename $@).list -o $@ $<
