unit main_form;

{$mode objfpc}{$H+}
{$R resources.rc}
//{$L libzfs.so}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, Grids, StdCtrls,
  Buttons, ExtCtrls, qt5, qtwidgets, qtobjects, Types, Process, create_form, LCLType,
  FileUtil, about_form, settings_form;

type
  libzfs_handle_p = pointer;

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
    procedure UpdateSnapshots();
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }


procedure AddIconToButton(Name: WideString; var Button: TBitBtn);
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
  ASize.cx := 24;
  ASize.cy := 24;

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
  SSName := SnapshotList.Rows[SnapshotList.Row][0];
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

procedure TMainForm.AboutItemClick(Sender: TObject);
begin
  AboutForm.Top := MainForm.Top + Round((MainForm.Height - AboutForm.Height) / 2);
  AboutForm.Left := MainForm.Left + Round((MainForm.Width - AboutForm.Width) / 2);
  AboutForm.ShowModal();
end;

procedure TMainForm.DeleteButtonClick(Sender: TObject);
var
  Output, SSName, Message: ansistring;
  Reply, BoxStyle: integer;
begin
  SSName := SnapshotList.Rows[SnapshotList.Row][0];

  BoxStyle := MB_ICONQUESTION + MB_YESNO;
  Message := 'Are you sure you wish to delete ' + SSName + '?';
  Reply := Application.MessageBox(PChar(Message), 'Delete a Snapshot?', BoxStyle);

  if (Reply <> idYes) then
    Exit();

  if RunCommand('zfs', ['destroy', SSName], Output,
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
      ShowMessage('Creating snapshot failed. Please make sure the ZFS tools are in your path.');
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
  SSName := SnapshotList.Rows[SnapshotList.Row][0];

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
  Output: ansistring;
  a, b: TStringArray;
  i: integer;
  ss, size, ref: string;
begin

  for i := SnapshotList.RowCount - 1 downto 1 do
    SnapshotList.DeleteRow(i);

  // I *really hate* this, but the libzfs API is ridiculous.  If you look at
  // the code for the "zfs list -t snapshot" command, you'll see that you have
  // to implement zfs_for_each() which takes a callback, which calls itself
  // recursively, and then another userland callback is called for each
  // row of output.  If I was in C, I'd just copy that code and move on,
  // But I'm not down for porting it to free pascal at the moment.
  if RunCommand('zfs', ['list', '-t', 'snapshot'], Output,
    [poUsePipes, poStderrToOutPut]) then
  begin
    a := Output.Split(AnsiChar(#10));
    for i := 1 to Length(a) - 2 do
    begin
      b := a[i].Split(' ', TStringSplitOptions.ExcludeEmpty);
      ss := b[0];
      size := b[1];
      ref := b[3];
      SnapshotList.InsertRowWithValues(i, [ss, size, ref]);
    end;
  end
  else
  begin
    if (Length(Output) <> 0) then
      ShowMessage(Output)
    else
      ShowMessage('Listing snapshot failed. Please make sure the ZFS tools are in your path.');
  end;

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
begin
  CurrentRow := SnapshotList.Row;
  UpdateSnapshots();
  if (CurrentRow < SnapshotList.RowCount) then
    SnapshotList.Row := CurrentRow
  else
    SnapshotList.Row := 1;
end;

procedure TMainForm.TrayIcon1Click(Sender: TObject);
begin
  if (WindowState = wsMinimized) then
    Application.Restore()
  else
    Application.Minimize();
end;


end.
