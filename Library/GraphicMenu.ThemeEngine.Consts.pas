{
Copyright (C) 2006-2021 Matteo Salvi

Website: http://www.salvadorsoftware.com/

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit GraphicMenu.ThemeEngine.Consts;

{$MODE DelphiUnicode}

interface

const
  //Theme.ini structure
  //Sections
  INIFILE_SECTION_INFO         = 'info';
  INIFILE_SECTION_GENERAL      = 'general';
  INIFILE_SECTION_RIGHTBUTTONS = 'rightbuttons';
  INIFILE_SECTION_HARDDISK     = 'harddisk';
  INIFILE_SECTION_SEARCH       = 'search';
  INIFILE_SECTION_LIST         = 'list';
  INIFILE_SECTION_RECENTS      = 'recent';
  INIFILE_SECTION_MOSTUSED     = 'mostused';
  INIFILE_SECTION_EJECTBUTTON  = 'ejectbutton';
  INIFILE_SECTION_EXITBUTTON   = 'exitbutton';
  //Keys
  //Info
  INIFILE_KEY_NAME    = 'name';
  INIFILE_KEY_AUTHOR  = 'author';
  INIFILE_KEY_VERSION = 'version';
  INIFILE_KEY_URL     = 'url';
  //Images
  INIFILE_KEY_IMAGEBUTTON  = 'image_button';
  INIFILE_KEY_IMAGEBACKGROUND = 'image_background';
  INIFILE_KEY_IMAGEUSERFRAME  = 'image_userframe';
  INIFILE_KEY_IMAGELOGO    = 'image_logo';
  INIFILE_KEY_IMAGESPACE   = 'image_space';
  INIFILE_KEY_IMAGESEPARATOR = 'image_separator';
  //Fonts
  INIFILE_KEY_FONTNORMAL   = 'font_normal';
  INIFILE_KEY_FONTHOVER    = 'font_hover';
  INIFILE_KEY_FONTCLICKED  = 'font_clicked';
  INIFILE_KEY_FONTDISABLED = 'font_disabled';
  INIFILE_KEY_FONTTREE     = 'font_tree';
  INIFILE_KEY_FONT         = 'font'; //Generic font key
  //Icons
  INIFILE_KEY_ICONASUITE   = 'icon_asuite';
  INIFILE_KEY_ICONEXPLORE  = 'icon_explore';
  INIFILE_KEY_ICONDOCUMENT = 'icon_document';
  INIFILE_KEY_ICONMUSIC    = 'icon_music';
  INIFILE_KEY_ICONPICTURES = 'icon_pictures';
  INIFILE_KEY_ICONVIDEOS   = 'icon_videos';
  INIFILE_KEY_ICONOPTIONS  = 'icon_options';
  INIFILE_KEY_ICONHELP     = 'icon_help';
  INIFILE_KEY_ICONSEARCH   = 'icon_search';
  INIFILE_KEY_ICONCANCEL   = 'icon_cancel';
  INIFILE_KEY_ICON         = 'icon'; //Generic icon key

implementation

end.
