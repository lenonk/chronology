unit create_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, Contnrs, Process, LCLType, ChronoUtility;

type

  { TCreateForm }

  TCreateForm = class(TForm)
    CancelButton: TBitBtn;
    CreateButton: TBitBtn;
    GroupBox1: TGroupBox;
    FileSystemListBox: TListBox;
    AvailData: TLabel;
    MountPointData: TLabel;
    MountPointLabel: TLabel;
    ReferData: TLabel;
    UsedLabel: TLabel;
    UsedData: TLabel;
    Panel1: TPanel;
    AvailLabel: TLabel;
    ReferLabel: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure CreateButtonClick(Sender: TObject);
    procedure FileSystemListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    DataMap: TFPObjectHashTable;

  public

  end;

  TFSData = class(TObject)
    Used: string;
    Avail: string;
    Refer: string;
    MountPoint: string;
  end;

var
  CreateForm: TCreateForm;

implementation

{$R *.lfm}

{ TCreateForm }

procedure TCreateForm.FormShow(Sender: TObject);
var
  Output: ansistring;
  a, b: TStringArray;
  i: Integer;
  Data: TFSData;
begin
    FileSystemListBox.Clear();

    if RunCommand('zfs',['list', '-t', 'filesystem'], Output,
                                 [poUsePipes, poStderrToOutPut]) then begin
       a := Output.Split(AnsiChar(#10));

       for i := 1 to Length(a) - 2 do begin
           Data := TFSData.Create();
           b := a[i].Split(' ', TStringSplitOptions.ExcludeEmpty);
           FileSystemListBox.Items.Add(b[0]);
           Data.Used := b[1];
           Data.Avail := b[2];
           Data.Refer := b[3];
           Data.MountPoint := b[4];
           DataMap[b[0]] := Data;
       end;
    end
    else begin
        if (Length(Output) <> 0) then ShowMessage(Output)
        else ShowMessage('Creating snapshot failed. Please make sure the ZFS tools are in your path.');
    end;

    FileSystemListBox.Selected[0] := true;
end;

procedure TCreateForm.FormCreate(Sender: TObject);
begin
  DataMap := TFPObjecthashTable.Create();
end;

procedure TCreateForm.FileSystemListBoxSelectionChange(Sender: TObject;
  User: boolean);
begin
    with Sender as TListBox do begin
        with DataMap[GetSelectedText()] as TFSData do begin
            UsedData.Caption := Used;
            AvailData.Caption := Avail;
            ReferData.Caption := Refer;
            MountPointData.Caption := MountPoint;
        end;
    end;
end;

procedure TCreateForm.CreateButtonClick(Sender: TObject);
var
  Output, SSName: ansistring;
  Reply: Integer;
begin
  Reply := Application.MessageBox(
        PChar('Are you sure you wish to create a new snapshot of ' + FileSystemListBox.GetSelectedText() + '?'),
        'Create a New Snapshot?', MB_ICONQUESTION + MB_YESNO);

  if (Reply <> IDYES) then begin
    ModalResult := mrNone;
    Exit();
  end;

  ModalResult := mrNone;
  if (CreateSnapshot(FileSystemListBox.GetSelectedText(), srManual, Output)) then
     ModalResult := mrYes
  else
    Application.MessageBox(PChar(Output), 'Chronology - Error', MB_ICONERROR + MB_OK);
end;

procedure TCreateForm.CancelButtonClick(Sender: TObject);
begin
  Close();
end;

procedure DataMapCallBack(Item: TObject; const Key: string; var Continue: Boolean);
begin
  with Item as TFSData do
       Destroy();
  Continue := true;
end;

procedure TCreateForm.FormDestroy(Sender: TObject);
begin
  DataMap.Iterate(@DataMapCallback);
end;

end.

