unit main_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, Grids, StdCtrls,
  Buttons, ExtCtrls, Types, Process, create_form, LCLType,
  FileUtil, about_form, settings_form, JSonTools, ChronoUtility, DateUtils;

type

  { TMainForm }

  TMainForm = class(TForm)
    Label1: TLabel;
    BrowseMenuItem: TMenuItem;
    RestoreMenuItem: TMenuItem;
    DeleteMenuItem: TMenuItem;
    NumSnapShotsLabel: TLabel;
    CreateButton: TBitBtn;
    AboutItem: TMenuItem;
    ActiveLabel: TLabel;
    LatestSnapshotData: TLabel;
    OldestSnapshotData: TLabel;
    LatestSnapsot: TLabel;
    OldestSnapshot: TLabel;
    SnapshotsPopup: TPopupMenu;
    ShieldImage: TImage;
    ConfigureItem: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    DolphinProcess: TProcess;
    MenuButtonMenu: TPopupMenu;
    Panel4: TPanel;
    Panel5: TPanel;
    RestoreButton: TBitBtn;
    DeleteButton: TBitBtn;
    BrowseButton: TBitBtn;
    SettingsButton: TBitBtn;
    Timer1: TTimer;
    TrayIcon1: TTrayIcon;
    WizardButton: TBitBtn;
    MenuButton: TBitBtn;
    SnapshotList: TStringGrid;
    procedure AboutItemClick(Sender: TObject);
    procedure BrowseButtonClick(Sender: TObject);
    procedure BrowseMenuItemClick(Sender: TObject);
    procedure ConfigureItemClick(Sender: TObject);
    procedure CreateButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure DeleteMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuButtonClick(Sender: TObject);
    procedure RestoreButtonClick(Sender: TObject);
    procedure RestoreMenuItemClick(Sender: TObject);
    procedure SettingsButtonClick(Sender: TObject);
    procedure SnapshotListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);

  private
    procedure UpdateSnapshots();
    function GetDatasetName(): ansistring;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

function TMainForm.GetDatasetName(): ansistring;
begin
  with SnapshotList do
       Result := Rows[Row][0] + '@' + Rows[Row][5];
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  TrayIcon1.Show();
end;

procedure TMainForm.CreateButtonClick(Sender: TObject);
begin
  CreateForm.ShowModal();
end;

procedure TMainForm.BrowseButtonClick(Sender: TObject);
var
  Output, SSName, UName, DirName: ansistring;
  GUID: TGUID;
begin
  SSName := GetDatasetName();
  UName := GetEnvironmentVariable('USER');
  CreateGuid(GUID);
  DirName := '/run/media/' + UName + '/' + GUID.ToString(True);

  if (not DirectoryExists(DirName)) then
  begin
    if (not ForceDirectories(DirName)) then
    begin
      ShowMessage('Could not create: ' + DirName);
      Exit();
    end;
  end;

  if (not RunCommand('mount', ['-t', 'zfs', SSName, DirName],
    Output, [poUsePipes, poStderrToOutput])) then
  begin
    if (Length(Output) <> 0) then
      ShowMessage(Output);
    Exit();
  end;

  DolphinProcess.Executable := FindDefaultExecutablePath('dolphin');
  DolphinProcess.Parameters.Add(DirName);
  DolphinProcess.Execute();

  if (not RunCommand('umount', [SSName], Output,
    [poUsePipes, poStderrToOutput])) then
  begin
    if (Length(Output) <> 0) then
      ShowMessage(Output);
    Exit();
  end;

  if (DirectoryExists(DirName)) then
  begin
    if (not RemoveDir(DirName)) then
    begin
      ShowMessage('Could not remove: ' + DirName);
      Exit();
    end;
  end;
end;

procedure TMainForm.BrowseMenuItemClick(Sender: TObject);
begin
  BrowseButtonClick(Sender);
end;

procedure TMainForm.ConfigureItemClick(Sender: TObject);
begin
  SettingsButton.Click();
end;

procedure TMainForm.AboutItemClick(Sender: TObject);
begin
  AboutForm.Top := MainForm.Top + Round((MainForm.Height - AboutForm.Height) / 2);
  AboutForm.Left := MainForm.Left + Round((MainForm.Width - AboutForm.Width) / 2);
  AboutForm.ShowModal();
end;

procedure TMainForm.DeleteButtonClick(Sender: TObject);
var
  Reply: Integer;
  Error: ansistring;
begin
  Reply := Application.MessageBox(
      PChar('Are you sure you wish to delete ' + GetDatasetName() + '?'),
      'Delete a Snapshot?', MB_ICONQUESTION + MB_YESNO);

  if (Reply <> idYes) then Exit();

  if not DeleteSnapshot(GetDatasetName(), Error) then begin
    Application.MessageBox(PChar(Error), 'Chronology - Error', MB_ICONERROR + MB_OK);
  end;
end;

procedure TMainForm.DeleteMenuItemClick(Sender: TObject);
begin
  DeleteButtonClick(Sender);
end;

procedure TMainForm.RestoreButtonClick(Sender: TObject);
var
  Output, SSName, Message: ansistring;
  Reply, BoxStyle: integer;
begin
  SSName := GetDatasetName();

  BoxStyle := MB_ICONQUESTION + MB_YESNO;
  Message := 'Are you sure you wish to restore ' + SSName + '?';
  Reply := Application.MessageBox(PChar(Message), 'Restore a Snapshot?', BoxStyle);

  if (Reply <> idYes) then
    Exit();

  if RunCommand('zfs', ['rollback', '-r', SSName], Output,
    [poUsePipes, poStderrToOutPut]) then
  begin
    if (Length(Output) <> 0) then
      ShowMessage(Output);
  end
  else
  begin
    if (Length(Output) <> 0) then
      ShowMessage(Output)
    else
      ShowMessage('Restoring snapshot failed. Please make sure the ZFS tools are in your path.');
  end;
end;

procedure TMainForm.RestoreMenuItemClick(Sender: TObject);
begin
  RestoreButtonClick(Sender);
end;

procedure TMainForm.SettingsButtonClick(Sender: TObject);
begin
  SettingsForm.Top := MainForm.Top + Round((MainForm.Height - SettingsForm.Height) / 2);
  SettingsForm.Left := MainForm.Left + Round((MainForm.Width - SettingsForm.Width) / 2);
  SettingsForm.ShowModal();
end;

procedure TMainForm.SnapshotListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Integer;
begin
  if (Button = TMouseButton.mbRight) then begin
    SnapshotList.MouseToCell(X, Y, Col, Row);
    SnapshotList.Row := Row;
    SnapshotsPopup.PopUp();
  end;
end;

procedure TMainForm.UpdateSnapshots();
var
  Error: ansistring;
begin
  if not ListSnapshots(SnapshotList, Error) then
    Application.MessageBox(PChar(Error), 'Chronology - Error', MB_ICONERROR + MB_OK);

  NumSnapshotsLabel.Caption := IntToStr(SnapshotList.RowCount - 1);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  AddIconToButton('document-save-public', CreateButton);
  AddIconToButton('document-open-recent-symbolic', RestoreButton);
  AddIconToButton('edit-delete-symbolic', DeleteButton);
  AddIconToButton('folder-symbolic', BrowseButton);
  AddIconToButton('preferences-system-symbolic', SettingsButton);
  AddIconToButton('emblem-default-symbolic', WizardButton);
  AddIconToButton('open-menu-symbolic', MenuButton);

  UpdateSnapshots();
end;

procedure TMainForm.MenuButtonClick(Sender: TObject);
var
  MenuLoc: TPoint;
begin
  MenuLoc.X := MenuButton.Left;
  MenuLoc.Y := MenuButton.Top;
  MenuLoc := ClientToScreen(MenuLoc);

  MenuButtonMenu.PopUp(MenuLoc.X, MenuLoc.Y + MenuButton.Height);
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
var
  CurrentRow: integer;
  Config, Node, DSNode, SNode, SSNode: TJsonNode;
  ConfigDir, ConfigFile, Error: ansistring;
  SnapshotTime: TDateTime;
  SSTimeStamps: TStringList;

begin
  CurrentRow := SnapshotList.Row;
  UpdateSnapshots();
  if (CurrentRow < SnapshotList.RowCount) then
    SnapshotList.Row := CurrentRow
  else
    SnapshotList.Row := 1;

  if not GetConfigLocation(ConfigDir, ConfigFile, Error) then begin
    Application.MessageBox(PChar(Error), 'Chronology - Error', MB_ICONERROR + MB_OK);
    Exit();
  end;

  if (not FileExists(ConfigDir + ConfigFile)) then Exit();

  Config := TJsonNode.Create();
  Config.LoadFromFile(ConfigDir + ConfigFile);

  if ((Config.Find('monthly|enabled').Value.ToBoolean() = False)
      and (Config.Find('weekly|enabled').Value.ToBoolean() = False)
      and (Config.Find('daily|enabled').Value.ToBoolean() = False)
      and (Config.Find('hourly|enabled').Value.ToBoolean() = False)
      and (Config.Find('boot|enabled').Value.ToBoolean() = False)) then begin
    ShieldImage.Picture.LoadFromResourceName(HInstance, 'SHIELD-WARNING-ICON');
    ActiveLabel.Caption := 'No Snapshots are scheduled';
  end
  else if (Config.Find('datasets').Count = 0) then begin
    ShieldImage.Picture.LoadFromResourceName(HInstance, 'SHIELD-WARNING-ICON');
    ActiveLabel.Caption := 'No Snapshots are scheduled';
  end
  else begin
    ShieldImage.Picture.LoadFromResourceName(HInstance, 'SHIELD-OK-ICON');
    ActiveLabel.Caption := 'Chronology is active';
  end;

  if Config.Find('last_snapshot_time') <> nil then begin
    SnapshotTime := StrToDateTime(Config.Find('last_snapshot_time').Value.DeQuotedString('"'));
    LatestSnapshotData.Caption := FormatDateTime('mmmm d, yyyy hh:nn AM/PM', SnapshotTime);
  end
  else
    LatestSnapshotData.Caption := 'None';

  Node := Config.Find('datasets');
  if Node = nil then OldestSnapshotData.Caption := 'None'
  else begin
    SSTimeStamps := TStringList.Create();
    for DSNode in Node do begin
      for SNode in DSNode do begin
        for SSnode in SNode do begin
          SSTimeStamps.Add(SSNode.Name);
        end;
      end;
    end;

    SSTimeStamps.Sort();
    SnapshotTime := UnixToDateTime(StrToInt(SSTimeStamps[0]));
    OldestSnapshotData.Caption := FormatDateTime('mmmm d, yyyy hh:nn AM/PM', SnapshotTime);
    SSTimeStamps.Destroy();
  end;

  Config.Free();
end;

procedure TMainForm.TrayIcon1Click(Sender: TObject);
begin
  if (WindowState = wsMinimized) then
    Application.Restore()
  else
    Application.Minimize();
end;


end.
