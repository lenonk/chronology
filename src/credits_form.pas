unit credits_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ChronoUtility;

type

  { TCreditsForm }

  TCreditsForm = class(TForm)
    BackButton: TBitBtn;
    CloseButton: TBitBtn;
    Label1: TLabel;
    Memo1: TMemo;
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  CreditsForm: TCreditsForm;

implementation

{$R *.lfm}

{ TCreditsForm }

procedure TCreditsForm.FormShow(Sender: TObject);
begin
  AddIconToButton('go-previous-symbolic', BackButton, 16, 16);
end;

end.

