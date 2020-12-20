unit ChronoUtility;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, Grids, StdCtrls,
  Buttons, ExtCtrls, qt5, qtwidgets, qtobjects, Types, Process, LCLType,
  FileUtil;

procedure AddIconToButton(Name: WideString; var Button: TBitBtn; sx: Integer = 0; sy: Integer = 0);

function GetConfigLocation(out dir: ansistring; out fname:ansistring): boolean;

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

function GetConfigLocation(out dir: ansistring; out fname: ansistring): boolean;
var
  UID, UserName: ansistring;
  Buf: ansistring;
  Passwd: TextFile;
  A: TStringArray;

begin
  UserName := '';
  UID := GetEnvironmentVariable('PKEXEC_UID');

  if (UID = '') then begin
    if (Application.HasOption('u', 'user')) then
      UserName := Application.GetOptionValue('u', 'user')
    else
      UserName := GetEnvironmentVariable('USER')
  end
  else begin
    System.Assign(Passwd, '/etc/passwd');
    Reset(Passwd);
    while not EoF(Passwd) do begin
      Readln(Passwd, Buf);
        A := Buf.Split(':');
        if (A[2] = UID) then begin
          UserName := A[0];
          break;
        end;
    end;
    CloseFile(Passwd);
  end;

  if (UserName = '') then begin
     Application.MessageBox('Unable to find user name.  Aborting',
                                    'Chronology - Error', MB_ICONERROR + MB_OK);
     GetConfigLocation := false;
     Exit();
  end;

  dir := '/home/' + UserName + '/.config/chronology/';
  fname := 'settings.json';
  GetConfigLocation := true;
end;

end.

