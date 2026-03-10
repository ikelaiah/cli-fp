program cli_fp_gen;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils,
  CliFpGen.App;

begin
  try
    RunCliFpGen;
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
