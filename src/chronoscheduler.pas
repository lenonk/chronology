unit ChronoScheduler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JsonTools, ChronoUtility, DateUtils, LazLoggerBase;

type

  {TScheduler}

  TScheduler = class
    constructor Create;

  private
    Config: TJsonNode;
    DataSets: TStringList;
    StopEmail: boolean;

    function LoadConfig(): boolean;
    function FindDatasets(): boolean;

    procedure CheckSchedule(SchedName: ansistring; Dataset: ansistring);
  public
    procedure Run();

  end;

implementation
var
  Scheduler: TScheduler = nil;

{ TScheduler }

constructor TScheduler.Create();
begin
  if not(assigned(Scheduler)) then begin
    inherited;
    Config := TJsonNode.Create();
    Datasets := TStringList.Create();
    Scheduler := Self;
  end
  else begin
    Self := Scheduler;
  end;
end;

function TScheduler.LoadConfig(): boolean;
var
  ConfigDir, ConfigFile, Output: ansistring;
begin
  if not GetConfigLocation(ConfigDir, ConfigFile, Output) then begin
     Exit(false);
  end;

  if not FileExists(ConfigDir + ConfigFile) then
  begin
     LoadConfig := false;
     Exit();
  end;

  Config.LoadFromFile(ConfigDir + ConfigFile);

  LoadConfig := true;
end;

function TScheduler.FindDatasets(): boolean;
var
  Node: TJsonNode;
  i: Integer;
begin
  Node := Config.Find('datasets');

  if ((Node = nil) or (Node.Count <= 0)) then begin
    FindDatasets := false;
    Exit();
  end;

  for i := 0 to Node.Count - 1 do begin
    Datasets.Add(Node.Child(i).Name);
  end;

  FindDatasets := true;
end;

procedure TScheduler.CheckSchedule(SchedName: ansistring; Dataset: ansistring);
var
  DNode, SNode, SSNode: TJsonNode;
  Latest: TJsonNode;
  SSDue: TDateTime;
  Error: ansistring;
begin
  SNode := Config.Find(SchedName);
  if SNode = nil then Exit();
  if SNode.Find('enabled').Value.ToBoolean = false then begin
    debugln(SchedName + ' not enabled...');
    Exit();
  end;

  Latest := SNode.Find('latest');
  if Latest = nil then SSDue := Now
  else begin
    case SchedName of
      'monthly': SSDue := IncMonth(StrToDateTime(Latest.Value.DeQuotedString('"')));
      'weekly': SSDue := IncWeek(StrToDateTime(Latest.Value.DeQuotedString('"')));
      'daily': SSDue := IncDay(StrToDateTime(Latest.Value.DeQuotedString('"')));
      'hourly': SSDue := IncHour(StrToDateTime(Latest.Value.DeQuotedString('"')));
    end;
  end;

  // Nothing to do for this schedule
  if Now < SSDue then Exit()
  else begin
    debugln(SchedName +  ' schedule due.  Creating snapshot for dataset: ' + Dataset);
    if not CreateSnapshot(Dataset, srAutomatic, Error, SchedName) then begin
      debugln(Error);
      Exit();
    end;
  end;

  DNode := Config.Find('datasets|' + Dataset);
  if DNode = nil then Exit(); // Should never happen

  SSNode := DNode.Find(SchedName + '_snapshots');
  if SSnode = nil then Exit(); // Should also never happen

  // GTE below because a snapshot was just created that wll not be accounted for in SSNode.Count
  while SSNode.Count >= SNode.Find('keep').Value.ToInteger() do begin
    // Because of the nature of JsonTools, the oldest snapshots will be first.
    // So ss.Child(0) is the oldest snapshot for this dataset, etc
    debugln('Snapshot node: ' + SSnode.Child(0).Find('name').Value.DeQuotedString('"') + ' old.  Deleting');
    if not DeleteSnapshot(SSnode.Child(0).Find('name').Value.DeQuotedString('"'), Error) then debugln(Error);
    SSNode.Delete(0);
  end;
end;

procedure TScheduler.Run();
var
  i: Integer;
  ScheduleName: ansistring;
begin
  if not LoadConfig() then begin
    writeln('No config file found.  Nothing to do.');
    Exit();
  end;

  if not FindDatasets() then begin
    writeln('No datasets found in config file.  Nothing to do.');
    Exit();
  end;

  for i := 0 to DataSets.Count - 1 do begin
    // TODO: Figure out what to do with Boot schedules
    for ScheduleName in ['monthly', 'weekly', 'daily', 'hourly'] do
      CheckSchedule(ScheduleName, DataSets[i]);
  end;
end;

end.

