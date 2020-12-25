unit create_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, Contnrs, Process, LCLType, Grids, ChronoUtility;

type

  { TCreateForm }

  TCreateForm = class(TForm)
    CancelButton: TBitBtn;
    CreateButton: TBitBtn;
    DatasetGrid: TStringGrid;
    Label1: TLabel;
    Label2: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure CreateButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private

  public

  end;


var
  CreateForm: TCreateForm;

implementation

{$R *.lfm}

{ TCreateForm }

procedure TCreateForm.FormShow(Sender: TObject);
var
  Error: string;
begin
  if not ListDatasets(DatasetGrid, Error) then begin
     Application.MessageBox(PChar('Error listing datasets.'), 'Chronology - Error', MB_ICONERROR + MB_OK);
     Exit();
  end;
end;

procedure TCreateForm.CreateButtonClick(Sender: TObject);
var
  Output, SSName: ansistring;
  Reply: Integer;
begin
  Reply := Application.MessageBox(
        PChar('Are you sure you wish to create a new snapshot of ' + DatasetGrid.Rows[DatasetGrid.Row][1] + '?'),
        'Create a New Snapshot?', MB_ICONQUESTION + MB_YESNO);

  if (Reply <> IDYES) then begin
    ModalResult := mrNone;
    Exit();
  end;

  ModalResult := mrNone;
  if (CreateSnapshot(DatasetGrid.Rows[DatasetGrid.Row][1], srManual, Output)) then
     ModalResult := mrYes
  else
    Application.MessageBox(PChar(Output), 'Chronology - Error', MB_ICONERROR + MB_OK);
end;

procedure TCreateForm.CancelButtonClick(Sender: TObject);
begin
  Close();
end;


end.

