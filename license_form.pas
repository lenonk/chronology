unit license_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, ChronoUtility;

type

  { TLicenseForm }

  TLicenseForm = class(TForm)
    BackButton: TBitBtn;
    CloseButton: TBitBtn;
    Label1: TLabel;
    Memo1: TMemo;
    procedure BackButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  LicenseForm: TLicenseForm;

implementation

{$R *.lfm}

{ TLicenseForm }

procedure TLicenseForm.FormShow(Sender: TObject);
begin
  AddIconToButton('go-previous-symbolic', BackButton, 16, 16);
end;

procedure TLicenseForm.BackButtonClick(Sender: TObject);
begin

end;

end.

