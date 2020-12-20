unit about_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  LCLIntf, Buttons, license_form, credits_form, ChronoUtility;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    CloseButton: TBitBtn;
    CreditsButton: TBitBtn;
    Label4: TLabel;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LicenseButton: TBitBtn;
    procedure CloseButtonClick(Sender: TObject);
    procedure CreditsButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure LicenseButtonClick(Sender: TObject);
  private

  public

  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.CloseButtonClick(Sender: TObject);
begin
  Close();
end;

procedure TAboutForm.CreditsButtonClick(Sender: TObject);
var
  Reply: Integer;
begin
  CreditsForm.Top := AboutForm.Top + Round((AboutForm.Height - CreditsForm.Height) / 2);
  CreditsForm.Left := AboutForm.Left + Round((AboutForm.Width - CreditsForm.Width) / 2);
  Reply := CreditsForm.ShowModal();

  if (Reply = mrClose) then Close();

end;

procedure TAboutForm.FormShow(Sender: TObject);
begin
    Label4.Font.Color := clBlue;
    Label4.Invalidate();
    Label4.Update();
    Label4.Repaint();

    AddIconToButton('help-about-symbolic', LicenseButton, 16, 16);
    AddIconToButton('help-about-symbolic', CreditsButton, 16, 16);
end;

procedure TAboutForm.Label4Click(Sender: TObject);
begin
  OpenURL('https://github.com/lenonk/chronology');
end;

procedure TAboutForm.LicenseButtonClick(Sender: TObject);
var
  Reply: Integer;
begin
  LicenseForm.Top := AboutForm.Top + Round((AboutForm.Height - LicenseForm.Height) / 2);
  LicenseForm.Left := AboutForm.Left + Round((AboutForm.Width - LicenseForm.Width) / 2);
  Reply := LicenseForm.ShowModal();

  if (Reply = mrClose) then Close();
end;

end.

