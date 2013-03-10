Software: KGrid component for Delphi and Lazarus
Original authorship: Tomas Krysl (tk@tkweb.eu)
-------------------


LICENSE:
-------------------
License information for each source file can be found in it's header.
If there is none, the code is public domain.


SYSTEM REQUIREMENTS:
-------------------
- platforms: Win32(98(SE)+), Win64(untested), GTK, GTK2, QT, Carbon(untested), WinCE(untested)
- works under Delphi 7 and higher (tested on Delphi 7, Delphi2007 till Delphi 2010) 
  and Lazarus 0.9.29(SVN#21827) and higher
- should work under Delphi 6
- some more problems might be experienced for older Lazarus versions


INSTALLATION:
-------------------
1. Compile and install package (it might be needed to specify some search paths in Delphi).
2. Put the TKGrid component into your application instead of TStringGrid or TDrawGrid.
3. When compiling an application, it might be needed to specify the search path to KGrid sources 
   or JCL sources (if JCL is configured).

PLANNED:
-------------------
- filters (still a TODO, partially can be implemented now via editable fixed rows)
- multiple disjunct selections (like Excel, still a TODO)
- tree columns

KNOWN PROBLEMS:
-------------------
Delphi common:
 -none, best performance

Lazarus common:
 -KDBGrid demo does not draw unicode fields correctly for certain databases. 
  applies for all data aware controls, not just TKDBgrid
  must be FPC/TWideStringField bug 
 -printing/Previewing works correctly in Win32 and Qt (for Lazarus versions with my implementation
  of affine transformations for device contexts). For GTKx printing via TPostScriptPrinterCanvas,
  there is no way to effectively implement affine transformations because this canvas is not implemented
  via a device context mechanism. 
  
Target specific:
Win32: 
 Delphi: 
  -none 
  -tested on Windows 98SE (some time ago), Windows XP 32bit, Windows Vista 32bit
 Lazarus: 
  -transparent editor underpainting incorrect if TKGrid is placed onto TPageControl (LCL problem)
  -tested on Windows XP 32bit
Win64: none
 -untested, help appreciated!
WinCE: 
 -slow inplace editor performance (depending on device)
 -tested partially, help appreciated!
GTK: 
 -bad check box painting, bad selected range color, drag window flickers, sometimes infinite painting, 
  sometimes clipping problems (all LCL/GTK problems)
 -tested on Ubuntu Jaunty
GTK2: 
 -slightly slow inplace editor performance with huge grids (GTK2 problem)
 -scrollbar arrows don't work correctly sometimes  (GTK2 problem)
 -tested on Ubuntu Jaunty
QT: 
 -slightly slow inplace editor performance with huge grids
 -checkbox not transparent (cannot be solved)
 -scrollbar arrows behave differently (cannot be solved)
 -tested on QT4.5.2/Windows XP
Carbon: 
 -none
 -untested, help appreciated!


TECHNICAL SUPPORT:
-------------------
Any suggestions, error reports and questions about this software please send to
the author or discuss on http://www.tkweb.eu.


CONTRIBUTORS:
-------------------
Gianluca Culot: idea for TKCustomGrid.OnChanged event
JR: some useful functions and ideas
aki: selectable fixed cells


VERSION HISTORY
-------------------
Version 1.7 (November 2010): 
  Added:
    -Windows Vista/7 style selection,
    -selectable and editable fixed cells (modified contibutions by aki)
    -packages for Delphi XE
  Modified:
    -removed some obsolete methods, several bugfixes

Version 1.6 (October 2010): 
  Added:
    -column/row/grid autosizing,
    -automatic data type recognition and images in TKDBGrid,
    -improvements in TKGridCellPainter (images, button shapes etc.),
	-cell hints
	-OnMouseDblClickCell event
    -new features based on contributions by JR (OptionsEx property)
	-PaintCell method
  Modified:
    -several bugfixes

Version 1.5 (October 2009): 
  Added:
    -printing/previewing/on the fly previewing (TKPrintPreview, TKPrintPageSetup classes etc.),
     in Lazarus works only for Win32(suppose Win64 too) widget set
    -OnMouseClickCell and OnMouseLeaveCell, OnMouseClickCell events
  Modified:
    -painting and inplace editor performance for GTK2, QT yet slightly improved

Version 1.4 (October 2009): 
  Added:
    -full Lazarus support (all official or beta state widget sets, tested on Win32/Win64, GTK, GTK2, QT)
    -cell merging and splitting (CellSpan property & TKGridCell ColSpan and RowSpan properties)
    -data aware control (TKDBGrid class)
    -column/row individual maximum and minimum extent (TKGridAxisItem MinExtent & MaxExtent properties)
    -smooth scrolling (ScrollModeHorz & ScrollModeVert properties)
    -OnMouseEnterCell and OnMouseLeaveCell events
    -KDBGrid demo for Delphi/Lazarus
  Modified:
    -HotFix 3.10: painting performance optimized for GTK2 
    -major modifications due to platform independency in Lazarus
    -some generous functions moved from KGrids.pas to KGraphics.pas or KFunctions.pas
    -(very) few incompatibilities with previous versions
    -KGrid demo extended
    -no more InnoSetup installation but generous zip package due to platform independency
    -lower case introduced for unit names etc. due to platform independency  
    -documentation completed

Version 1.3 (August 2009): 
  Added:
    -ported to Lazarus (Windows widgetset only)
    -TKCustomGrid.ThroughClick property (clicking a cell will click the inplace editor as well)
    -TKGridTextAttributes - text attributes (multiline text, end ellipsis, path ellipsis, word break)
    -keyboard behavior extended
  Modified:
    -JCL not needed anymore (mainly because of the Lazarus support)
    -inplace editor rendering
    -documentation

Version 1.3 beta (July 2009): 
  Added:
    -TKGridAxisItem.Visible property
    -optional visual indication of hidden columns or rows
    -goIndicateHiddenCells style in TKCustomGrid.Options
    -goMouseCanHideCells style in TKCustomGrid.Options
    -goHeaderAlignment style in TKCustomGrid.Options 
    -TKCustomGrid.SortStyle property
    -TKCustomGrid.UpdateSortMode method 
  Modified:
    -moving columns/rows via OnExchangeCols/OnExchangeRows (both normal and virtual mode)
    -inplace editor rendering
    -documentation
 
Version 1.2 (October 2008): 
  Added:
    -OnChanged event handler
  Modified:
    -update to Delphi 2009
    -painting of the themed header cells fixed
    -painting of some inplace editors fixed (e.g. TRichEdit)

Version 1.1 (April 2008): 
  Added:
    -sorting interface 
    -cell clipping and double buffering
    -TKGridCellPainter class, 
    -improved compatibility with TStringGrid
    -another small improvements and fixes
  Modified:
    -demo has been extended
    -documentation   

Version 1.0 (January 2008):
  Added:
    -index mapping
    -small demo, 
    -documentation
    -many other improvements and bug fixes

Version 0.9 (July 2007): Initial release