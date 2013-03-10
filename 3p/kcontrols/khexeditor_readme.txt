Software: KhexEditor component for Delphi and Lazarus
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
2. When compiling an application or demo, it might be needed to specify the search path to KHexEditor sources.


PLANNED:
-------------------
- improve performance for non-Win32 widget sets in Lazarus
- byte swaps (Little and Big Endian)
- considering: visual control of modified text


KNOWN PROBLEMS:
-------------------
Delphi common:
 -none, best performance

Lazarus common:
 -slow performance on non-Win32 widget sets 


TECHNICAL SUPPORT:
-------------------
Any suggestions, error reports and questions about this software please send to
the author or discuss on http://www.tkweb.eu.


VERSION HISTORY:
-------------------
Version 1.5 (November 2010)
  Added:
    -Append method to append data at a position
  Modified:
    -packages for Delphi XE

Version 1.4 (October 2009)
  Modified:
    -printing and previewing to comply with kcontrols.pas
    -update to Delphi 2010
    -port to Lazarus

Version 1.3 (October 2008)
  Modified:
    -update to Delphi 2009

Version 1.22 (January 2008)
  Modified:
    -packages included for newer Delphi

Version 1.21 (June 2006)
  Modified:
    -bugs fixed when no printer installed    

Version 1.2 (June 2006):
  Added:
    -runtime package
    -UpdateCharMetrics method
    -keyboard features into TKHexEditorPrintPreview
  Modified:
    -UpdateScrollRange modified to avoid
     design-time exceptions in the IDE
    -minor bugfixes

Version 1.1 (May 2006):
  Added:
    -print preview - new component (KHexEditorPreview.pas)
    -PaintTo method to paint the outline to another canvas
  Modified:
    -little modifications

Version 1.0 (April 2006): Initial release