{$INCLUDE Switches.pas}

unit Directories;

interface

var
  BinariesDirectory, AiDirectory, GraphicsDirectory, SoundsDirectory,
  UserDirectory: string;

function LocalizedFilePath(path: string): string;


implementation

uses
  ShlObj, Windows, SysUtils;

function GetSpecialDirectory(const CSIDL: integer): string;
var
  RecPath: PChar;
begin
  RecPath := StrAlloc(MAX_PATH);
  try
    FillChar(RecPath^, MAX_PATH, 0);
    if SHGetSpecialFolderPath(0, RecPath, CSIDL, False) then
      Result := RecPath
    else
      Result := '';
  finally
    StrDispose(RecPath);
  end;
end;

function DirectoryExists(path: string): boolean;
var
  f: TSearchRec;
begin
  Result := FindFirst(path, faDirectory, f) = 0;
end;

function LocalizedFilePath(path: string): string;
begin
  Result := UserDirectory + 'Localization\' + path;
  if not FileExists(Result) then
    Result := BinariesDirectory + path;
end;


var
  AppUserDirectory: string;
  src, dst: TSearchRec;

initialization
  BinariesDirectory := ExtractFilePath(ParamStr(0));
  AiDirectory := BinariesDirectory + 'AI\';
  GraphicsDirectory := BinariesDirectory + 'Graphics\';
  SoundsDirectory := BinariesDirectory + 'Sounds\';

  AppUserDirectory := GetSpecialDirectory(CSIDL_APPDATA);
  if AppUserDirectory = '' then
    UserDirectory := BinariesDirectory
  else
  begin
    if not DirectoryExists(AppUserDirectory + '\C-evo') then
      CreateDir(AppUserDirectory + '\C-evo');
    UserDirectory := AppUserDirectory + '\C-evo\';
  end;
  if not DirectoryExists(UserDirectory + 'Saved') then
    CreateDir(UserDirectory + 'Saved');
  if not DirectoryExists(UserDirectory + 'Maps') then
    CreateDir(UserDirectory + 'Maps');

  // copy appdata if not done yet
  if FindFirst(BinariesDirectory + 'AppData\Saved\*.cevo', $21, src) = 0 then
    repeat
      if (FindFirst(UserDirectory + 'Saved\' + src.Name, $21, dst) <> 0) or
        (dst.Time < src.Time) then
        CopyFile(PChar(BinariesDirectory + 'AppData\Saved\' + src.Name),
          PChar(UserDirectory + 'Saved\' + src.Name), False);
    until FindNext(src) <> 0;
end.
