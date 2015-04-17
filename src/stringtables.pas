{$INCLUDE switches.pas}

unit StringTables;

interface

const
  MaxCount = 4000;

type
  TCharList = array[0..9999999] of char;

  TStringTable = class
    constructor Create;
    destructor Destroy; override;
    function LoadFromFile(const FileName: string): boolean;
    function GetHandle(const Item: string): integer;
    function LookupByHandle(Handle: integer; Index: integer = -1): string;
    function Lookup(const Item: string; Index: integer = -1): string;
    function Search(const Content: string; var Handle, Index: integer): boolean;
  protected
    Count: integer;
    Data: ^TCharList;
    Lines: array[0..MaxCount - 1] of PChar;
  end;


implementation

uses
  Classes, SysUtils;

constructor TStringTable.Create;
begin
  Data := nil;
end;

destructor TStringTable.Destroy;
begin
  if Data <> nil then
    FreeMem(Data);
end;

function TStringTable.LoadFromFile(const FileName: string): boolean;
var
  nData, i: integer;
  f: TFileStream;
begin
  if Data <> nil then
    FreeMem(Data);
  try
    f := TFileStream.Create(FileName, fmOpenRead or fmShareExclusive);
  except
    Result := False;
    exit;
  end;
  Result := True;
  nData := f.Size;
  GetMem(Data, nData + 1);
  f.Read(Data^, nData);
  f.Free;
  i := 0;
  Count := 0;
  while (i < nData) and (Count < MaxCount) do
  begin
    Lines[Count] := @Data[i];
    while (i < nData) and (Data[i] <> #13) do
      Inc(i);
    Data[i] := #0;
    Inc(i, 2);
    Inc(Count);
  end;
end;

function TStringTable.GetHandle(const Item: string): integer;
var
  i, l: integer;
begin
  l := Length(Item);
  i := Count - 1;
  while (i >= 0) and ((Lines[i][0] <> '#') or
      (StrLComp(Lines[i] + 1, @Item[1], l) <> 0) or (Lines[i][l + 1] <> #0) and
      (Lines[i][l + 1] <> ' ')) do
    Dec(i);
  Result := i;
end;

function TStringTable.LookupByHandle(Handle: integer; Index: integer): string;
var
  s: string;
begin
  if Index < 0 then
    if Handle < 0 then
    begin
      Result := '';
      exit;
    end
    else
    begin
      if pos(' ', Lines[Handle]) = 0 then
        s := ''
      else
        s := copy(Lines[Handle], pos(' ', Lines[Handle]) + 1, MaxInt);
      while (Handle + 1 < Count) and (Lines[Handle + 1][0] <> '#') do
      begin
        Inc(Handle);
        if (Lines[Handle][0] <> #0) and (Lines[Handle][0] <> '''') then
        begin
          if (s <> '') and (s[Length(s)] <> '\') then
            s := s + ' ';
          s := s + Lines[Handle];
        end;
      end;
      Result := s;
    end
  else if Handle + Index + 1 >= Count then
  begin
    Result := '';
    exit;
  end
  else
    Result := Lines[Handle + Index + 1];
  while (Result <> '') and ((Result[1] = ' ') or (Result[1] = #9)) do
    Delete(Result, 1, 1);
  while (Result <> '') and ((Result[Length(Result)] = ' ') or
      (Result[Length(Result)] = #9)) do
    Delete(Result, Length(Result), 1);
  if Result = '' then
    Result := '*';
end;

function TStringTable.Lookup(const Item: string; Index: integer): string;
var
  Handle: integer;
begin
  Handle := Gethandle(Item);
  if Handle >= 0 then
    Result := LookupByHandle(Handle, Index)
  else
    Result := '';
  if Result = '' then
    if Index < 0 then
      Result := Format('[%s]', [Item])
    else
      Result := Format('[%s %d]', [Item, Index]);
end;

{might become necessary for 1.3

function TStringTable.Lookup(const Fallback: TStringTable; const Item: string; Index: integer): string;
var
Handle: integer;
begin
Handle:=Gethandle(Item);
if Handle>=0 then result:=LookupByHandle(Handle, Index)
else result:='';
if result='' then
  result:=Fallback.Lookup(Item, Index);
end;

function TStringTable.TryLookup(const Item: string; Index: integer): string;
var
Handle: integer;
begin
Handle:=Gethandle(Item);
if Handle>=0 then result:=LookupByHandle(Handle, Index)
else result:='';
end;}

function TStringTable.Search(const Content: string; var Handle, Index: integer): boolean;
var
  h, i: integer;
  UContent: string;
begin
  UContent := UpperCase(Content);
  h := Handle;
  if h < 0 then
    i := 0
  else
    i := Index + 1;
  repeat
    if h + i + 1 >= Count then
    begin
      Result := False;
      exit;
    end;
    if Lines[h + i + 1][0] = '#' then
    begin
      h := h + i + 1;
      i := -1;
    end;
    if (h >= 0) and not (Lines[h + i + 1][0] in ['#', ':', ';']) and
      (pos(UContent, UpperCase(Lines[h + i + 1])) > 0) then
    begin
      Index := i;
      Handle := h;
      Result := True;
      exit;
    end;
    Inc(i);
  until False;
end;

end.
