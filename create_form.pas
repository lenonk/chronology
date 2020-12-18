unit create_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, Contnrs, Process, LCLType;

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
  Output, SSName, Message: ansistring;
  Reply, BoxStyle: Integer;
begin
    ModalResult := mrNone;

    SSName := FileSystemListBox.GetSelectedText() + '@' +
           FormatDateTime('YYYY-MM-DD-HH.NN.AM/PM', Now);

    BoxStyle := MB_ICONQUESTION + MB_YESNO;
    Message := 'Are you sure you wish to create a new snapsot of ' +
                FileSystemListBox.GetSelectedText() + '?';

    Reply := Application.MessageBox(PChar(Message), 'Create a New Snapshot?', BoxStyle);

    if (Reply <> IDYES) then Exit();

    if RunCommand('zfs', ['snap', SSName], Output,
                         [poUsePipes, poStderrToOutPut]) then begin
       if (Length(Output) <> 0) then ShowMessage(Output);
       ModalResult := mrYes;
    end
    else begin
        if (Length(Output) <> 0) then ShowMessage(Output)
        else ShowMessage('Creating snapshot failed. Please make sure the ZFS tools are in your path.');
    end;
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

