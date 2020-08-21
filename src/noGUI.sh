#!/bin/bash
rm -rf noGUI
mkdir noGUI
cp ./Makefile_noGUI noGUI/Makefile
cp *.a noGUI
cp *.F noGUI
cp -r libreta_slow noGUI
cp -r libreta_fast noGUI
for f90 in *.f90
do
grep -a -v -E "use dislin_d|use plot|use GUI|call gifmod|call pngmod|call angle|call drawdomaingui|call drawmolgui|call drawplanegui|call drawisosurgui|call drawmoltopogui|call drawsurfanalysis|call drawbasinintgui|call drawmol|call drawcurve|call drawscatter|call drawmatcolor|call drawplane|call useclrind|metafl|call window|scrmod|imgfmt|call page|disini|hwfont|axslen|axspos|wintit|call ticks|errmod|labdig|call name|setgrf|call graf|setrgb|call dash|call grid|call solid|call curve|call color|box2d|endgrf|xaxgit|disfin|disini|call ticks|errmod|legini|legtit|call frame|legpos|leglin|legpat|swgfnt|setscr|symfil|messag|imgfmt|complx|sursze|call center|rlmess|call rline|call height|setxid|winsiz|autres|ax3len|crvmat|getscr|ticpos|call labels|call dot|winsiz|namdis|call linwid|call legend|call setboxgui|call selfilegui|call selcolor|call setcolor|call minigui|call texmod|call triplx|call legopt|call myline|call hname|call setgraphformat|call setfil|call SETVLT|call AXSSCL" $f90 -i > noGUI/$f90
done
cp Bspline.f90 noGUI
echo "Now you should run:"
echo "cd noGUI"
echo "make -j"
