program chronology;

{$mode objfpc}{$H+}

{$IFDEF NOGUI}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, ChronoUtility, ChronoScheduler, DateUtils
  { you can add units after this };

type

  { chronology_sched }

  chronology_sched = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ chronology_sched }

procedure chronology_sched.DoRun;
var
  ErrorMsg: String;
  UName, Message: ansistring;
  BoxStyle: integer;
  Scheduler: TScheduler;

begin
  // quick check parameters
  ErrorMsg := CheckOptions('hu:', ['help', 'user']);
  if ErrorMsg <> '' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }
  {UName := GetEnvironmentVariable('USER');
  if (UName <> 'root') then begin
    BoxStyle := MB_ICONERROR + MB_OK;
    Message := 'Admin accesss is required to backup and restore system files.' + sLineBreak +
               'Please re-run the application as admin (using ''sudo'', ''su'', or ''pkexec'').';

    Application.MessageBox(PChar(Message), 'Chronology - Error', BoxStyle);
    Application.Terminate();
  end;}

  Scheduler := TScheduler.Create();
  writeln('Running in scheduler mode: ', DateTimeToUnix(Now));
  Scheduler.Run();

  // stop program loop
  Terminate;
end;

constructor chronology_sched.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor chronology_sched.Destroy;
begin
  inherited Destroy;
end;

procedure chronology_sched.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: chronology_sched;
begin
  Application:=chronology_sched.Create(nil);
  Application.Title:='Chronology Scheduler';
  Application.Run;
  Application.Free;
end.
{$ELSE}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main_form, create_form, SysUtils, LCLType, lazcontrols,
  about_form, settings_form, license_form, ChronoUtility, credits_form,
  ChronoScheduler, DateUtils, LazLogger
  { you can add units after this };

{$R *.res}

var
  UName, Message: ansistring;
  BoxStyle: integer;
  Scheduler: TScheduler;

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;

  {UName := GetEnvironmentVariable('USER');
  if (UName <> 'root') then begin
    BoxStyle := MB_ICONERROR + MB_OK;
    Message := 'Admin accesss is required to backup and restore system files.' + sLineBreak +
               'Please re-run the application as admin (using ''sudo'', ''su'', or ''pkexec'').';

    Application.MessageBox(PChar(Message), 'Chronology - Error', BoxStyle);
    Application.Terminate();
  end;}

  if (Application.HasOption('s', 'scheduler')) then begin
    Scheduler := TScheduler.Create();
    writeln('Running in scheduler mode: ', DateTimeToUnix(Now));
    Scheduler.Run();
    Exit();
  end;

  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TCreateForm, CreateForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.CreateForm(TLicenseForm, LicenseForm);
  Application.CreateForm(TCreditsForm, CreditsForm);
  Application.Run;
end.
{$ENDIF}
