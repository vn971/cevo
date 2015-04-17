{$INCLUDE switches}

unit Directories;

interface

var
  BinariesDirectory, AiDirectory, GraphicsDirectory, SoundsDirectory,
  UserDirectory: string;

function LocalizedFilePath(path: string): string;


implementation

uses
  FileUtil, // copy files
  LCLIntf, LCLType, LMessages, // replacement for "windows" unit
  SysUtils;

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

  AppUserDirectory := GetAppConfigDir(False);
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
        FileUtil.CopyFile(
          BinariesDirectory + 'AppData\Saved\' + src.Name,
          UserDirectory + 'Saved\' + src.Name, False);
    until FindNext(src) <> 0;
end.
