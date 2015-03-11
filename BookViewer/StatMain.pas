{$INCLUDE ..\switches}

unit StatMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TMainForm = class(TForm)
    OpenDialog: TOpenDialog;
    List: TListBox;
    Panel1: TPanel;
    OpenBtn: TButton;
    procedure OpenBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure OpenBook(FileName: String);
  end;

var
  MainForm: TMainForm;

implementation

uses Protocol, CmdList;

{$R *.DFM}

const
lxmax=100; lymax=96;
nPl=15; {max number of players, don't change!}

// old version commands
sToggleCityTile=$7400; {+tile code shl 4}
  {reserves $74XX, $75XX}

type
TLogData=array[word] of Byte;

procedure TMainForm.OpenBtnClick(Sender: TObject);
begin
if opendialog.execute then OpenBook(OpenDialog.FileName);
end;

procedure TMainForm.OpenBook(FileName: String);
var
i,j,d,lx,ly,SaveID,nHeader,nTotal,nMapSize,Pos0,LandMass,ExeTime: integer;
Command, Player, Subject: integer;
Data: pointer;
SaveMap: array[0..lxmax*lymax+2] of cardinal;
s: string[255];
nstat, sstat: array[0..$FF] of integer;
statcommand: array[0..$FF] of integer;
LogFile: TFileStream;
CL: TCmdList;

begin
nMapSize:=0;
List.Clear;
List.Items.Add('');
LogFile:=TFileStream.Create(FileName,fmOpenRead or fmShareExclusive);
LogFile.Position:=0;
LogFile.Read(s[1],8); {file id}
s[0]:=#8;
if s<>'cEvoBook' then
  begin
  List.Items.Add('Invalid format');
  exit;
  end;
LogFile.Read(SaveID,4); {format id}
if SaveID<20 then
  begin
  List.Items.Add('C-evo version older than 0.7.2');
  exit;
  end
else if SaveID<$100 then
  List.Items.Add(Format(' Version:     %d',[SaveID]))
else List.Items.Add(Format(' Version:     %d.%d.%d',
  [SaveID shr 16, SaveID shr 8 and $FF, SaveID and $FF]));

if SaveID>=$000E01 then
  begin
  LogFile.Read(ExeTime,4);
  List.Items.Add(' Executable:  '
    +FormatDateTime('c', FileDateToDateTime(ExeTime)));
  end;

LogFile.Read(lx,4);
LogFile.Read(ly,4);
List.Items.Add(Format(' World Size:  %d Tiles',[lx*ly]));
LogFile.Read(LandMass,4);
if LandMass=0 then
  begin
  inc(nMapSize,lx*ly*4);
  LogFile.read(SaveMap,lx*ly*4) // use predefined map
  end
else
  List.Items.Add(Format(' Landmass:    %d%%',[Landmass]));
if SaveID>=27 then
  begin
  LogFile.Read(d,4);
  List.Items.Add(Format(' End Turn:    %d',[d]));
  end;
LogFile.Read(d,4);
LogFile.Read(d,4);
List.Items.Add(Format(' Last Turn:   %d',[d]));
LogFile.read(SaveMap,4);
if SaveMap[0]<>$80 then
  begin
  inc(nMapSize,((lx*ly-1) div 4+1)*4);
  LogFile.read(SaveMap[4],((lx*ly-1) div 4+1)*4-4);
  end;

for i:=0 to nPl-1 do
  begin
  LogFile.Read(s[0],4);
  if s[0]>#0 then
    begin
    LogFile.Read(s[4],Byte(s[0]) div 4 *4);
    if SaveID>=27 then
      LogFile.Read(d,4);{data version}
    LogFile.Read(d,4);{behavior}
    LogFile.Read(d,4); {difficulty}
    List.Items.Add(Format(' Player %2d:   %s (%d)',[i,s,d]));
    end;
  end;

nHeader:=LogFile.Position+4;
CL:=TCmdList.Create;
CL.LoadFromFile(LogFile);
LogFile.Free;

FillChar(nstat,sizeof(nstat),0);
FillChar(sstat,sizeof(sstat),0);

while CL.Progress<1000 do
  begin
  Pos0:=CL.State.LoadPos;
  CL.Get(Command, player, Subject, Data);
  if player<0 then player:=16;
  case Command of
    sSetDevModelCap..sSetDevModelCap+$3F0: Command:=sSetDevModelCap;
    sMoveUnit..sMoveUnit+$3F0: Command:=sMoveUnit;
    sStartJob..sStartJob+$3F0: Command:=sStartJob;
    sToggleCityTile..sToggleCityTile+$1F0: Command:=sToggleCityTile;
    end;
  if Command=sIntDataChange then CL.GetDataChanges(nil, 0);
  inc(nstat[Command shr 8]);
  inc(sstat[Command shr 8],CL.State.LoadPos-Pos0);
  end;
nTotal:=CL.State.LoadPos;
CL.Free;

// only one toggle
inc(nstat[$74],nstat[$75]); nstat[$75]:=0;
inc(sstat[$74],sstat[$75]); sstat[$75]:=0;

// sort by size
for i:=0 to $FF do statcommand[i]:=i;
for i:=0 to $FE do for j:=i+1 to $FF do
  if sstat[i]<sstat[j] then
    begin
    d:=sstat[i]; sstat[i]:=sstat[j]; sstat[j]:=d;
    d:=nstat[i]; nstat[i]:=nstat[j]; nstat[j]:=d;
    d:=statcommand[i]; statcommand[i]:=statcommand[j]; statcommand[j]:=d;
    end;

List.Items.Add('');
List.Items.Add(' Command              Count       Size');
List.Items.Add(' ----------------------------------------------');
List.Items.Add(Format(' Header                     %10.0n (%.1f%%)',
  [nHeader-nMapSize+0.,(nHeader-nMapSize)*100/(nTotal+nHeader)]));
if LandMass=0 then
  List.Items.Add(Format(' Map                        %10.0n (%.1f%%)',
    [lx*ly*4+0.,(lx*ly*4)*100/(nTotal+nHeader)]));
if SaveMap[0]<>$80 then
  List.Items.Add(Format(' Preview Map                %10.0n (%.1f%%)',
    [((lx*ly-1) div 4+1)*4+0.,(((lx*ly-1) div 4+1)*4)*100/(nTotal+nHeader)]));
for i:=0 to $FF do if nstat[i]>0 then
  begin
  case statcommand[i] of
    $40,$43: s:=Format('Internal %xXX',[statcommand[i]]);
    $41: s:='TellAboutModel';
    $42: s:='ExpandTerritory';
    $44: s:='Trades';
    $45: s:='SetDevModel';
    $46: s:='Status Changes';
    $47: s:='Data Changes';
    $48: s:='Turn';
    $51: s:='SetGov/SetRates';
    $52: s:='SetResearch';
    $53: s:='SetAttitude';
    $60: s:='RemoveUnit';
    $61: s:='SetUnitHome';
    $62: s:='Load/UnloadUnit';
    $64: s:='MoveUnit';
    $6C: s:='StartJob';
    $70: s:='SetCityProject';
    $72: s:='SetCityTiles';
    $80..$FF: s:=Format('ClientEx %xXX',[statcommand[i]]);
    else s:=Format('%xXX',[statcommand[i]]);
    end;
  List.Items.Add(Format(' %-16s %9.0n %10.0n (%.1f%%)',
    [s,nstat[i]+0.,sstat[i]+0.,sstat[i]*100/(nTotal+nHeader)]));
  end;
List.Items.Add(Format(' Total                      %10.0n (%.1f%%)',
  [nTotal+nHeader+0.,100.]));
List.Items.Add('');
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
if paramcount>0 then OpenBook(ParamStr(1));
end;

end.

