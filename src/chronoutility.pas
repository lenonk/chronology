unit ChronoUtility;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Grids, Buttons, qt5, qtwidgets, Types, Process, LCLType,
  FileUtil, DateUtils, JsonTools, LazLoggerBase, StrUtils;

type
  TSnapshotReason = (srAutomatic, srManual);

procedure AddIconToButton(Name: WideString; var Button: TBitBtn; sx: Integer = 0; sy: Integer = 0);
Procedure QSort(numbers :Array of Integer; left: Integer; right: Integer);

function GetConfigLocation(out dir: ansistring; out fname:ansistring; out Error: ansistring): boolean;
function CreateSnapshot(Dataset: ansistring; Reason: TSnapshotReason; out Error: ansistring;
  Schedule: ansistring = ''): boolean;
function DeleteSnapshot(SSName: ansistring; out Error: ansistring): boolean;
function ListDatasets(var Grid: TStringGrid; out Error: ansistring): boolean;
function ListSnapshots(var Grid: TStringGrid; out Error: ansistring): boolean;
function GetSnapshotType(ss: ansistring): ansistring;
function GetSnapshotDate(ss: ansistring): ansistring;
operator not(n: TJsonNode): Boolean;

implementation

operator not(n: TJsonNode): Boolean;
begin
  Result := n = nil;
end;

operator in(s: ansistring; a: array of ansistring): Boolean;
var
  b: ansistring;
begin
  Result := false;
  for b in a do begin
    Result := s = b;
    if Result then break;
  end;
end;

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

procedure QSort(numbers :Array of Integer; left: Integer; right: Integer);
Var
	pivot, l_ptr, r_ptr : Integer;

Begin
	l_ptr := left;
	r_ptr := right;
	pivot := numbers[left];

	while left < right do
	begin
		while ((numbers[right] >= pivot) and (left < right)) do
			right := right - 1;

		if left <> right then
		begin
			numbers[left] := numbers[right];
			left := left + 1;
		end;

		while ((numbers[left] <= pivot) and (left < right)) do
			left := left + 1;

		if left <> right then
		begin
			numbers[right] := numbers[left];
			right := right - 1;
		end;
	end;

	numbers[left] := pivot;
	pivot := left;
	left := l_ptr;
	right := r_ptr;

	if left < pivot then
		QSort(numbers, left, pivot-1);

	If right > pivot then
		QSort(numbers, pivot+1, right);
end;

function GetConfigLocation(out dir: ansistring; out fname: ansistring; out Error: ansistring): boolean;
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
    Error := 'Unable to find user name. Aborting.';
    debugln(Error);
    Exit(false);
  end;

  dir := '/home/' + UserName + '/.config/chronology/';
  fname := 'settings.json';
  Result := true;
end;

function CreateSnapshot(Dataset: ansistring; Reason: TSnapshotReason;  out Error: ansistring; Schedule: ansistring = ''): boolean;
var
  Output, SSName: ansistring;
  Config, Node, SSNode: TJsonNode;
  ConfigDir, ConfigFile: ansistring;
begin
  if not GetConfigLocation(ConfigDir, ConfigFile, Output) then begin
    debugln(Output);
    Exit(false);
  end;

  if Reason = srAutomatic then
    SSName := Dataset + '@automatic.' + Schedule + '.' + IntToStr(DateTimeToUnix(Now))
  else
    SSName := Dataset + '@manual.' + IntToStr(DateTimeToUnix(Now));

  if RunCommand('zfs', ['snap', SSName], Output, [poUsePipes, poStderrToOutPut]) then begin
     Result := true;
   end
   else begin
     if (Length(Output) <> 0) then begin
       Error := Output;
       debugln(Output);
     end
     else begin
       Error := 'Creating snapshot failed. Please make sure the ZFS tools are in your path.';
       debugln(Error);
     end;

     Exit(false);
   end;

  if (Reason = srManual) then Exit(true);

  Config := TJsonNode.Create();
  Config.LoadFromFile(ConfigDir + ConfigFile);

  Node := Config.Find('datasets' + '|' + Dataset);
  if Node = nil then Exit(true);

  SSNode := Node.Find(Schedule + '_snapshots');
  if SSNode = nil then SSNode := Node.Add(Schedule + '_snapshots');

  SSNode := SSNode.add(IntToStr(DateTimeToUnix(Now)));
  SSNode.Add('name', SSName);
  SSNode.Add('flags').Value := '["A"]';

  SSNode := Config.Find(Schedule);
  if SSNode <> nil then SSNode.Add('latest', DateTimeToStr(Now));

  Config.Add('last_snapshot_time', DateTimeToStr(Now));

  Config.SaveToFile(ConfigDir + ConfigFile);
  Config.Free();

  Result := true;
end;

function DeleteSnapshot(SSName: ansistring; out Error: ansistring): boolean;
var
  Output: ansistring;
  Config, Node, SSNode, TNode: TJsonNode;
  ConfigDir, ConfigFile: ansistring;
begin
  if RunCommand('zfs', ['destroy', SSName], Output, [poUsePipes, poStderrToOutPut]) then Result := true
  else begin
      if (Length(Output) <> 0) then begin
        Error := Output;
        debugln(Output);
      end
      else begin
        Error := 'Deleting snapshot failed. Please make sure the ZFS tools are in your path.';
        debugln(Error);
      end;

      Exit(false);
  end;

  if not GetConfigLocation(ConfigDir, ConfigFile, Error) then begin
    debugln(Error);
    Exit(true);
  end;

  if not FileExists(ConfigDir + ConfigFile) then Exit(true);

  Config := TJsonNode.Create();
  Config.LoadFromFile(ConfigDir + ConfigFile);

  Output :=SSname.Substring(0, SSName.IndexOf('@'));
  Node := Config.Find('datasets' + '|' + SSname.Substring(0, SSName.IndexOf('@')));
  if Node = nil then Exit(true);

  for SSNode in Node do begin
    for TNode in SSNode do begin
        if TNode.Find('name').Value.DeQuotedString('"') = SSName then begin
          SSNode.Delete(TNode.Name);
          break;
        end;
    end;
  end;

  Config.SaveToFile(ConfigDir + ConfigFile);
  Config.Free();

  Result := true;
end;

function ListDatasets(var Grid: TStringGrid; out Error: ansistring): boolean;
var
  i: Integer;
  Output: ansistring;
  a, b: TStringArray;
begin
  for i := Grid.RowCount - 1 downto 1 do Grid.DeleteRow(i);

  if RunCommand('zfs', ['list', '-t', 'filesystem'], Output, [poUsePipes, poStderrToOutPut]) then
  begin
    a := Output.Split(AnsiChar(#10));
    for i := 1 to Length(a) - 2 do
    begin
      b := a[i].Split(' ', TStringSplitOptions.ExcludeEmpty);
      Grid.InsertRowWithValues(i, ['0', b[0], b[1], b[2], b[3], b[4]]);
    end;
    Result := true;
  end
  else
  begin
    if (Length(Output) <> 0) then Error := Output
    else Error := 'Listing snapshot failed. Please make sure the ZFS tools are in your path.';
    Result := false;
  end;
end;

function GetSnapshotType(ss: ansistring): ansistring;
var
  Reasons: array[0..1] of ansistring = ('automatic', 'manual');
  Schedules: array[0..4] of ansistring = ('monthly', 'weekly', 'daily', 'hourly', 'boot');
  Idx: Integer;
  a: TStringArray;
begin
  a := ss.Split('.');
  if (Length(a) <= 1) and not (a[0] in Reasons) and not (a[1] in Schedules) then Exit('-');

  if a[0] = 'manual' then Idx := 0
  else Idx := 1;

  Result := AnsiProperCase(a[Idx], [' ']);
end;

function GetSnapshotDate(ss: ansistring): ansistring;
var
  Reasons: array[0..1] of ansistring = ('automatic', 'manual');
  Schedules: array[0..4] of ansistring = ('monthly', 'weekly', 'daily', 'hourly', 'boot');
  Idx: Integer;
  a: TStringArray;
begin
  a := ss.Split('.');
  if (Length(a) <= 1) and not (a[0] in Reasons) and not (a[1] in Schedules) then Exit(ss);

  if a[0] = 'manual' then Idx := 1
  else Idx := 2;

  Result := DateTimeToStr(UnixToDateTime(StrToInt(a[Idx])));
end;

function ListSnapshots(var Grid: TStringGrid; out Error: ansistring): boolean;
var
  i: Integer;
  Output: ansistring;
  a, b: TStringArray;
begin
  for i := Grid.RowCount - 1 downto 1 do
    Grid.DeleteRow(i);

  if RunCommand('zfs', ['list', '-t', 'snapshot'], Output, [poUsePipes, poStderrToOutPut]) then
  begin
    a := Output.Split(AnsiChar(#10));
    for i := 1 to Length(a) - 2 do
    begin
      b := a[i].Split(' @', TStringSplitOptions.ExcludeEmpty);
      Grid.InsertRowWithValues(i, [b[0], GetSnapshotType(b[1]), GetSnapshotDate(b[1]), b[2], b[4], b[1]]);
    end;
    Result := true;
  end
  else
  begin
    if (Length(Output) = 0) then Error := Output
    else Error := 'Listing snapshot failed. Please make sure the ZFS tools are in your path.';
    Result := false;
  end;
end;

end.

