{$INCLUDE switches}

unit Directories;

interface

uses
ShlObj,Windows,SysUtils;

var
HomeDir, DataDir: string;


implementation

function GetSpecialDirectory(const CSIDL: integer): string;
var
RecPath: PChar;
begin
RecPath:=StrAlloc(MAX_PATH);
try
  FillChar(RecPath^, MAX_PATH, 0);
  if SHGetSpecialFolderPath(0, RecPath, CSIDL, false) then
    result:=RecPath
  else result:='';
finally
  StrDispose(RecPath);
  end
end;

function DirectoryExists(path: string): boolean;
var
f: TSearchRec;
begin
result:=FindFirst(path,faDirectory,f)=0;
end;


var
AppDataDir: string;

initialization
HomeDir:=ExtractFilePath(ParamStr(0));


AppDataDir:=GetSpecialDirectory(CSIDL_APPDATA);
if AppDataDir='' then
  DataDir:=HomeDir
else
  begin
  if not DirectoryExists(AppDataDir+'\C-evo') then
    CreateDir(AppDataDir+'\C-evo');
  DataDir:=AppDataDir+'\C-evo\';
  end;
if not DirectoryExists(DataDir+'Saved') then
  CreateDir(DataDir+'Saved');
if not DirectoryExists(DataDir+'Maps') then
  CreateDir(DataDir+'Maps');
end.
