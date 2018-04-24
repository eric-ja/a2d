#!/bin/bash

set -e

function cecho {
    case $1 in
        red)    tput setaf 1 ;;
        green)  tput setaf 2 ;;
        yellow) tput setaf 3 ;;
    esac
    echo -e "$2"
    tput sgr0
}

function do_make {
    make $MAKE_FLAGS "$1" "$2" >/dev/null \
        && (cecho green "make $1 good") \
        || (tput blink ; cecho red "MAKE $1 BAD" ; return 1)
}

function verify {
    do_make "VERSION=$3" "out-$3/$2";
    diff "orig/$1" "out-$3/$2" \
        && (cecho green "diff $2 $3 $1 good" ) \
        || (
            tput blink ; 
            cecho red "DIFF $2 $3 $1 BAD" ; 
            
            od -Ax -tx1z "orig/$1" > /tmp/good ;
            od -Ax -tx1z "out-$3/$2" > /tmp/bad ;
            diff -u /tmp/good /tmp/bad;
            rm /tmp/good /tmp/bad ; 
            return 1
        )
}

function stats {
    echo "$(printf '%-20s' $1)""$(../res/stats.pl < $1)"
}

COMMON="mgtk"
SOURCES="
controls.s
cursor.s
dispatch.s
drawicon.s
events.s
fillblit.s
fixeddiv.s
framepoly.s
graf.s
hirestables.s
keyboardmouse.s
lines.s
menus.s
mouse.s
paintbits.s
polybufs.s
polygons.s
rect.s
setmenuselection.s
setzp.s
shifttables.s
start.s
stdport.s
text.s
version.s
windows.s
zp.s
zpsave.s
"

# Verify original and output match
echo "Verifying diffs:"
verify DESKTOP2_mgtk     mgtk 1.0.0f1-all
verify MBP_tk_abs        mgtk 1.0.0b10-all
verify FM_tk_abs         mgtk 1.0.0b5-all
verify KYANADV_prim_abs  mgtk 1.0.0b5-prim
verify UNIV2_mgtk        mgtk.pascal 1.0.0b5-menu


echo "Style check:"
TRAILINGWS=`find . \( -name "*.s" -o -name "*.inc" \) -type f -exec egrep -l " +$" {} \;`
if [ "$TRAILINGWS" ] ; then
    cecho red "Trailing WS in\n$TRAILINGWS" ; exit 1
fi
HARDTAB=`find . \( -name "*.s" -o -name "*.inc" \) -type f -exec egrep -l $'\t' {} \;`
if [ "$HARDTAB" ] ; then
    cecho red "Hard tab in\n$HARDTAB" ; exit 1
fi

cecho green "okay"

# Compute stats
echo "Stats:"
for t in $SOURCES; do
    stats "$t"
done;
