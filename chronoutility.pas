unit ChronoUtility;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, Grids, StdCtrls,
  Buttons, ExtCtrls, qt5, qtwidgets, qtobjects, Types, Process, LCLType,
  FileUtil;

procedure AddIconToButton(Name:WideString; var Button: TBitBtn; sx: Integer = 0; sy: Integer = 0);

implementation

procedure AddIconToButton(Name: WideString; var Button: TBitBtn; sx: Integer = 0; sy: Integer = 0);
var
  AIcon: QIconH;
  BIcon: QIconH;
  IconName: pwidestring;
  ASize: TSize;
  APixmap: QPixmapH;
begin
  New(IconName);
  AIcon := QIcon_create();
  BIcon := QIcon_create();
  APixmap := QPixmap_create();

  if sx = 0 then ASize.cx := 24 else ASize.cx := sx;
  if sy = 0 then ASize.cy := 24 else ASize.cy := sy;

  IconName^ := Name;
  QIcon_fromTheme(AIcon, IconName, nil); // Add fallback
  QIcon_pixmap(AIcon, APixmap, PSize(@ASize));
  QIcon_addPixmap(BIcon, APixmap, QIconNormal, QIconOn);
  TQTBitBtn(Button.Handle).setIconSize(@ASize);
  TQTBitBtn(Button.Handle).setIcon(BIcon);

  QIcon_destroy(AIcon);
  QIcon_destroy(BIcon);
  QPixmap_destroy(APixmap);
  Dispose(IconName);
end;

end.

