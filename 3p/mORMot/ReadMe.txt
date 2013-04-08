
  Synopse mORMot framework

An Open Source Client-Server ORM/SOA framework
 (c) 2008-2013 Synopse Informatique
  http://synopse.info
  http://mormot.net


Synopse mORMot is a Client-Server ORM and Service Oriented Architecture framework for Delphi 6 up to XE3, for Win32 and Win64 platforms.

The main two features of mORMot are therefore:
- Client-Server ORM: objects persistence and remote access;
- Client-Server Services: remote call of high-level data process.

Due to its modular design, switch from such a Client-Server architecture over HTTP, named pipes or GDI messages into a stand-alone application is just a matter of mORMot classes initialization. For instance, the very same executable can even be running stand-alone, as a server, or a client, depending on some run-time parameters!

Emphasizing simplicity, speed and versatility, mORMot is a incredibly well documented Open Source project easy enough to add basic ORM or Client-Server features to simple applications for hobbyists, or let experienced users develop scaling and strong service-based projects for their customers, with the advantages of native code and easy-to-deploy solutions, reducing deployment cost and increasing ROI.

With mORMot, ORM is not used only for data persistence of objects (like in other implementations), but as part of a global n-Tier, Service Oriented Architecture (SOA), ready to implement Domain-Driven solutions. 
This really makes the difference.

The framework Core is non visual (only a set of classes), but you have also some User Interface units available (including reporting and ribbon GUI), and you can use it in any RAD or AJAX clients.

Licensed under a disjunctive tri-license giving you the choice of one of the three following sets of free software/open source licensing terms:
- Mozilla Public License, version 1.1 or later;
- GNU General Public License, version 2.0 or later;
- GNU Lesser General Public License, version 2.1 or later.
This allows the use of our code in as wide a variety of software projects as possible, while still maintaining copyleft on code we wrote.

Main project page:
http://synopse.info/fossil/wiki?name=SQLite3+Framework

How to get the source:
http://synopse.info/fossil/wiki?name=Get+the+source

Download links and documentation:
http://synopse.info/fossil/wiki?name=Downloads

A forum is dedicated to this framework:
http://synopse.info

A blog is available:
http://blog.synopse.info

Issues and feature requests can be posted (but take a look at the forum and latest unstable version first!):
http://synopse.info/fossil/reportlist


Don't forget to download the documentation (available in pdf files, created by our SynProject tool).
In particular, you should take a look at all general introduction chapters of the supplied SAD document. It will cover all key-concepts and code modeling used by the framework.
A developer guide is included in this SAD document, in its 2nd part. You'll get good practice guidance, presentation of the ORM/SOA approach and other underlying concepts.

Enjoy!


Quick steps to install mORMot:

0) Read the licenses at http://synopse.info/forum/viewtopic.php?id=27
   Using any part of this project implies that you accept the terms of one of those licenses.

1) Download:
- Stable version (less features and bug fixes) from http://synopse.info/fossil/wiki?name=Downloads
- Latest trunk version (preferred) from http://synopse.info/fossil/wiki?name=Get+the+source (do not forget to get also the latest *.obj files)

2) There is no install program, so unzip the files (including sub folders) to your Delphi component directory.
   Example: D:\Dev\Synopse\

3a) If you use an IDE before Delphi 2006, you need the FastMM4.pas memory manager to run the tests - see http://sourceforge.net/projects/fastmm to get a copy, and to be installed in your Delphi library path (e.g. D:\Dev\Synopse); if you do not want to use it, just modify SynDprUses.inc to contain nothing

3b) If you use a Delphi XE2/XE3 and targets Windows 64, you need an external Sqlite3 library - get it from http://synopse.info/files/SQLite3-64.7z

4) Modify the Delphi IDE library path to include:
   D:\Dev\Synopse;D:\Dev\Synopse\SQLite3;D:\Dev\Synopse\SynDBDataset

5) Compile the project in D:\Dev\Synopse\Sqlite3\TestSQL3.dpr and run it to make sure it passes all tests.
   On some computers, named pipes communication tests may fail - see http://synopse.info/forum/viewtopic.php?id=678
   If you want to run the tests with the fast http.sys kernel-based HTTP server, you'll need to compile and run (as administrator) TestSQL3Register.dpr once before launching TestSQL3.dpr

6) Sample programs are found in: D:\Dev\Synopse\Sqlite3\Samples

7) Download the mORMot documentation from http://synopse.info/fossil/wiki?name=Downloads
   In particular, the SAD document is worth reading - consult the keyword index at the beginning if you are somewhat lost about terminology

8) After having consulted both the documentation and the existing posts in the forum, feel free to ask your questions in the forum at http://synopse.info

9) Feel free to contribute by posting enhancements and patches to this quickly evolving project.


Some units (like SynPdf, SynGdiPlus, SynBigTable, SynCommons, SynDB*, SynSQLite3, mORMotReport) are used by mORMot, but do not require the whole framework to be used.
That is, you can use e.g. only the PDF generation features of SynPdf, the fast database access classes of SynDB, the static-linked SQLite3 engine of SynSQLite3, the code-generated reports of mORMotReport, or the TDynArray / TSynLog classes of SynCommons, without using the main mORMot classes and features (ORM, Client-Server, services, UI, reporting).
Some of those units can even be compiled with Delphi 5 (like SynPdf, SynDB or SynSQLite3).