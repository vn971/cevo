{$INCLUDE Switches.pas}

unit Settings;

interface

var
  windowed: boolean = False;
  printHelp: boolean = False;
  popupEnabled: boolean = True;

implementation

var
  i: integer;

initialization
  for i := 1 to Paramcount do
  begin
    if (ParamStr(i) = '--help') or (ParamStr(i) = '-h') then
    begin
      printHelp := True;
      WriteLn('debug: --help argument found');
    end
    else if (ParamStr(i) = '--windowed') or (ParamStr(i) = '-w') then
    begin
      windowed := True;
      WriteLn('debug: --windowed argument found');
    end
    else if (ParamStr(i) = '--no-popup') or (ParamStr(i) = '-p') then
    begin
      popupEnabled := False;
      WriteLn('debug: --no-popup argument found');
    end;
  end;
end.
