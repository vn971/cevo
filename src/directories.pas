{$INCLUDE switches.pas}

unit Directories;

interface

var
  BinariesDirectory,
  AiDirectory, GraphicsDirectory, SoundsDirectory,
  GraphicsFileExtension,
  UserDirectory: String;

function LocalizedFilePath(path: string): string;


implementation

uses
  FileUtil,
  LCLIntf, LCLType,
  SysUtils;

function LocalizedFilePath(path: string): string;
begin
  Result := UserDirectory + 'Localization' + DirectorySeparator + path;
  if not FileExistsUTF8(Result) then
    Result := BinariesDirectory + path;
end;


var
  src, dst: TSearchRec;

initialization
  GraphicsFileExtension := '.bmp';
  BinariesDirectory := ExtractFilePath(ParamStr(0));
  AiDirectory := BinariesDirectory + 'AI' + DirectorySeparator;
  GraphicsDirectory := BinariesDirectory + 'Graphics' + DirectorySeparator;
  SoundsDirectory := BinariesDirectory + 'Sounds' + DirectorySeparator;

  UserDirectory:=GetAppConfigDir(False);
  CreateDirUTF8(UserDirectory);
  if not DirectoryExists(UserDirectory + 'Saved') then
    CreateDir(UserDirectory + 'Saved');
  if not DirectoryExists(UserDirectory + 'Maps') then
    CreateDir(UserDirectory + 'Maps');

  // copy appdata if not done yet
  if FindFirst(BinariesDirectory + 'AppData' +
     DirectorySeparator + 'Saved' +
     DirectorySeparator + '*.cevo', $21, src) = 0
  then repeat
      if (FindFirst(UserDirectory + 'Saved' + DirectorySeparator + src.Name, $21, dst) <> 0) or
        (dst.Time < src.Time) then
        FileUtil.CopyFile(
          BinariesDirectory + 'AppData' + DirectorySeparator + 'Saved' + DirectorySeparator + src.Name,
          UserDirectory + 'Saved' + DirectorySeparator + src.Name, False);
    until FindNext(src) <> 0;
end.
