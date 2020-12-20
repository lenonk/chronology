unit ChronoScheduler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JsonTools, ChronoUtility;

type

  {TScheduler}

  TScheduler = class
  private
    class var
      Config: TJsonNode;

      function LoadConfig(): boolean;

  public
    class var
      procedure Run();
  end;

implementation

{ TScheduler }

function TScheduler.LoadConfig(): boolean;
var
  ConfigDir, ConfigFile: ansistring;
begin
  if (not GetConfigLocation(ConfigDir, ConfigFile)) then begin
     LoadConfig := false;
     Exit();
  end;

  if (not FileExists(ConfigDir + ConfigFile)) then
  begin
     LoadConfig := false;
     Exit();
  end;

  Config := TJsonNode.Create();
  Config.LoadFromFile(ConfigDir + ConfigFile);
  Config.Free();

  LoadConfig := true;
end;

procedure TScheduler.Run();
begin
  if (not LoadConfig()) then begin
    writeln('No config file found.  Nothing to do.');
    Exit();
  end;

end;

end.

