program chronology;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main_form, create_form, SysUtils, LCLType, lazcontrols, about_form,
  settings_form
  { you can add units after this };

{$R *.res}

var
  UName, Message: ansistring;
  BoxStyle: integer;

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  UName := GetEnvironmentVariable('USER');
  {if (UName <> 'root') then begin
    BoxStyle := MB_ICONERROR + MB_OK;
    Message := 'Admin accesss is required to backup and restore system files.' + sLineBreak +
               'Please re-run the application as admin (using ''sudo'', ''su'', or ''pkexec'').';

    Application.MessageBox(PChar(Message), 'Admin Access Required', BoxStyle);
    Application.Terminate();
  end;}
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TCreateForm, CreateForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.Run;
end.

