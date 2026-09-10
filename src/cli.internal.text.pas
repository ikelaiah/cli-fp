unit CLI.Internal.Text;

{$mode objfpc}{$H+}{$J-}

interface

function SanitizeTerminalText(const Text: string): string;

implementation

function SanitizeTerminalText(const Text: string): string;
var
  i: Integer;
begin
  Result := Text;
  for i := Length(Result) downto 1 do
    if (Result[i] = #0) or (Result[i] = #27) then
      Delete(Result, i, 1);
end;

end.
