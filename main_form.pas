unit main_form;

{$mode objfpc}{$H+}
//{$L libzfs.so}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, Grids, StdCtrls,
  Buttons, ctypes, ExtCtrls, qt5, qtwidgets, Types, Process, create_form, LCLType;

type libzfs_handle_p = pointer;
type

  { TMainForm }

    TMainForm = class(TForm)
    CreateButton: TBitBtn;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    RestoreButton: TBitBtn;
    DeleteButton: TBitBtn;
    BrowseButton: TBitBtn;
    SettingsButton: TBitBtn;
    Timer1: TTimer;
    TrayIcon1: TTrayIcon;
    WizardButton: TBitBtn;
    MenuButton: TBitBtn;
    SnapshotList: TStringGrid;
    procedure CreateButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);
    procedure UpdateSnapshots();
  private
       //m_zfsh: libzfs_handle_p;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

{type
    PFILE = pointer;

    zfs_handle = record
        libzfs_error: cint;
        libzfs_fd: cint;
        libzfs_mnttab: PFILE;
        libzfs_sharetab: PFILE;
        libzfs_pool_handles: pointer;
        libzfs_ns_avlpool: pointer;
        libzfs_ns_avl: pointer;
        libzfs_ns_gen: cuint64;
        libzfs_desc_active: cint;
        libzfs_action: array [0..1023] of cchar;
        libzfs_desc: array [0..1023] of cchar;
        libzfs_printerr: cint;
        libzfs_storeerr: cint;
        libzfs_sharehdl: pointer;
        libzfs_shareflags: cuint;
        libzfs_mnttab_enable: cbool;
        libzfs_mnttab_cache_lock: pointer; // broken
        libzfs_mntab_cache: pointer;       // broken
        libzfs_pool_iter: cint;
        libzfs_chassis_id: array [0..255] of cchar;
        libzfs_prop_debug: cbool;
        libzfs_dedup_warning_printed: cbool;
    end;

    zfs_handle_t = zfs_handle;
    pzfs_handle_t = ^zfs_handle_t;

    zfs_iter_f = function (Handle:pzfs_handle_t; data:pointer):longint;cdecl;

function libzfs_init(): libzfs_handle_p; cdecl; external name 'libzfs_init';
procedure libzfs_fini(lp: libzfs_handle_p); cdecl; external name 'libzfs_fini';

function zfs_iter_snapshots(Handle: pzfs_handle_t; Simple: cbool;
         callback: zfs_iter_f; data: pointer; min_txg: cuint64;
         max_txg: cuint64):
         longint; cdecl; external name 'zfs_iter_snapshots';}

procedure TMainForm.CreateButtonClick(Sender: TObject);
begin
    CreateForm.ShowModal();
end;

procedure TMainForm.DeleteButtonClick(Sender: TObject);
var
  Output, SSName, Message: ansistring;
  Reply, BoxStyle: Integer;
begin
    SSName := SnapshotList.Rows[SnapshotList.Row][0];

    BoxStyle := MB_ICONQUESTION + MB_YESNO;
    Message := 'Are you sure you wish to delete ' + SSName + '?';
    Reply := Application.MessageBox(PChar(Message), 'Delete a Snapshot?', BoxStyle);

    if (Reply <> IDYES) then Exit();

    if RunCommand('zfs', ['destroy', SSName], Output,
                         [poUsePipes, poStderrToOutPut]) then begin
       if (Length(Output) <> 0) then ShowMessage(Output);
       ModalResult := mrYes;
    end
    else begin
        if (Length(Output) <> 0) then ShowMessage(Output)
        else ShowMessage('Creating snapshot failed. Please make sure the ZFS tools are in your path.');
    end;
end;

procedure AddIconToButton(Name: widestring; var Button: TBitBtn);
var AIcon: QIconH;
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

    AddIconToButton('document-save-public', CreateButton);
    AddIconToButton('document-open-recent-symbolic', RestoreButton);
    AddIconToButton('edit-delete-symbolic', DeleteButton);
    AddIconToButton('folder-symbolic', BrowseButton);
    AddIconToButton('preferences-system-symbolic', SettingsButton);
    AddIconToButton('emblem-default-symbolic', WizardButton);
    AddIconToButton('open-menu-symbolic', MenuButton);

    //m_zfsh := libzfs_init();
    {m_zfsh := nil;
    if (m_zfsh = nil) then begin
        ShowMessage('Failed to initialize ZFS library. Aborting.');
        Close();
    end;}

    UpdateSnapshots();
    TrayIcon1.Show();
end;

{function zfs_callback(Handle:pzfs_handle_t; data:pointer):longint; cdecl;
begin
    zfs_callback := 0;
end;}

procedure TMainForm.UpdateSnapshots();
var
    Output: ansistring;
    a, b: TStringArray;
    i: Integer;
    ss, size, ref: string;
begin

    for i := SnapshotList.RowCount - 1 downto 1 do
        SnapshotList.DeleteRow(i);

    {if (m_zfsh <> nil) then
        zfs_iter_snapshots(m_zfsh, TRUE, @zfs_callback, @SnapshotList, 0, 0);}
    // I *really hate* this, but the libzfs API is ridiculous.  If you look at
    // the code for the "zfs list -t snapshot" command, you'll see that you have
    // to implement zfs_for_each() which takes a callback, which calls itself
    // recursively, and then another userland callback is called for each
    // row of output.  If I was in C, I'd just copy that code and move on,
    // But I'm not down for porting it to free pascal at the moment.
    if RunCommand('zfs',['list', '-t', 'snapshot'], Output,
                                 [poUsePipes, poStderrToOutPut]) then begin
       a := Output.Split(AnsiChar(#10));
       for i := 1 to Length(a) - 2 do begin
           b := a[i].Split(' ', TStringSplitOptions.ExcludeEmpty);
           ss := b[0]; size := b[1]; ref := b[3];
           SnapshotList.InsertRowWithValues(i, [ss, size, ref]);
       end;
    end
    else begin
        if (Length(Output) <> 0) then ShowMessage(Output)
        else ShowMessage('Creating snapshot failed. Please make sure the ZFS tools are in your path.');
    end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
    {if (m_zfsh <> nil) then
        libzfs_fini(m_zfsh);}
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
var
    CurrentRow: Integer;
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
    if (WindowState = wsMinimized) then begin
        Application.Restore();
    end
    else begin
        Application.Minimize();
    end;
end;


end.

