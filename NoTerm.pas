{$INCLUDE switches}

unit NoTerm;

interface

uses
  ScreenTools,Protocol,

  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ButtonBase, ButtonB;

type
  TNoTermDlg = class(TForm)
    QuitBtn: TButtonB;
    GoBtn: TButtonB;
    procedure GoBtnClick(Sender: TObject);
    procedure QuitBtnClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
  public
    procedure Client(Command, Player: integer; var Data);
  private
    me, Active, ToldAlive, Round: integer;
    G: TNewGameData;
    Server: TServerCall;
    Shade, State: TBitmap;
    WinStat, ExtStat: array[0..nPl-1] of integer;
    Mode: (Stop, Stopped, Running, Quit);
    procedure NewStat;
    procedure EndPlaying;
    procedure ShowActive(p: integer; Active: boolean);
    procedure ShowYear;
    procedure OnEraseBkgnd(var m:TMessage); message WM_ERASEBKGND;
    procedure OnHitTest(var Msg:TMessage); message WM_NCHITTEST;
  end;

var
  NoTermDlg: TNoTermDlg;

procedure Client(Command,Player:integer;var Data); stdcall;

implementation

uses GameServer, log, Start, Messg;

{$R *.DFM}

const
nPlOffered=9;
x0Brain=109; y0Brain=124;
dxBrain=80; dyBrain=80;
xBrain: array[0..nPlOffered-1] of integer =
  (x0Brain,x0Brain,x0Brain+dxBrain,x0Brain+dxBrain,x0Brain+dxBrain,x0Brain,
  x0Brain-dxBrain,x0Brain-dxBrain,x0Brain-dxBrain);
yBrain: array[0..nPlOffered-1] of integer =
  (y0Brain,y0Brain-dyBrain,y0Brain-dyBrain,y0Brain,y0Brain+dyBrain,
  y0Brain+dyBrain,y0Brain+dyBrain,y0Brain,y0Brain-dyBrain);
xActive: array[0..nPlOffered-1] of integer = (0,0,24,34,24,0,-24,-34,-24);
yActive: array[0..nPlOffered-1] of integer = (0,-34,-24,0,24,34,24,0,-24);

var
FormsCreated: boolean;

procedure TNoTermDlg.OnEraseBkgnd(var m:TMessage);
begin
end;

procedure TNoTermDlg.OnHitTest(var Msg:TMessage);
begin
if (Msg.LParamHi>=Top+36) or (Msg.LParamLo>=Left+QuitBtn.Left)
  {or (Msg.LParamLo<Left+StatBtn.Left+StatBtn.Width)} then Msg.result:=HTCLIENT
else Msg.result:=HTCAPTION
end;

procedure TNoTermDlg.FormCreate(Sender: TObject);
begin
Left:=Screen.Width-Width-8; Top:=8;
Caption:=Phrases.Lookup('AIT');
Canvas.Brush.Style:=bsClear;
Canvas.Font.Assign(UniFont[ftSmall]);
InitButtons(self);
end;

procedure TNoTermDlg.NewStat;
begin
Round:=0;
FillChar(WinStat,SizeOf(WinStat),0);
FillChar(ExtStat,SizeOf(ExtStat),0);
Mode:=Stop;
end;

procedure TNoTermDlg.EndPlaying;
var
EndCommand: integer;
begin
NewStat;
if G.RO[me].Turn>0 then with MessgDlg do
  begin
  MessgText:=Phrases.Lookup('ENDTOUR');
  Kind:=mkYesNo;
  ShowModal;
  if ModalResult=mrIgnore then EndCommand:=sResign
  else EndCommand:=sBreak
  end
else EndCommand:=sResign;
Server(EndCommand,me,0,nil^)
end;

procedure TNoTermDlg.ShowActive(p: integer; Active: boolean);
begin
if p>=nPlOffered then exit;
BitBlt(Canvas.Handle,x0Brain+28+xActive[p],y0Brain+28+yActive[p],8,8,
  GrExt[HGrSystem].Mask.Canvas.Handle,81+9*Byte(Active),16,SRCAND);
BitBlt(Canvas.Handle,x0Brain+28+xActive[p],y0Brain+28+yActive[p],8,8,
  GrExt[HGrSystem].Data.Canvas.Handle,81+9*Byte(Active),16,SRCPAINT);
end;

procedure TNoTermDlg.ShowYear;
begin
Fill(State.Canvas,0,0,192,20,64,290);
RisedTextOut(State.Canvas,0,0,Format(Phrases.Lookup('AIT_ROUND'),[Round])+' '
  +TurnToString(G.RO[me].Turn));
BitBlt(Canvas.Handle,64,290,192,20,State.Canvas.Handle,0,0,SRCCOPY);
end;

procedure TNoTermDlg.Client(Command, Player: integer; var Data);
var
i,x,y,p: integer;
ShipComplete: boolean;
r: TRect;
begin
case Command of
  cDebugMessage:
    LogDlg.Add(Player, G.RO[0].Turn, pchar(@Data));

  cInitModule:
    begin
    Server:=TInitModuleData(Data).Server;
    TInitModuleData(Data).Flags:=aiThreaded;
    Shade:=TBitmap.Create;
    Shade.Width:=64; Shade.Height:=64;
    for x:=0 to 63 do for y:=0 to 63 do
      if Odd(x+y) then Shade.Canvas.Pixels[x,y]:=$FFFFFF
      else Shade.Canvas.Pixels[x,y]:=$000000;
    State:=TBitmap.Create;
    State.Width:=192; State.Height:=20;
    State.Canvas.Brush.Style:=bsClear;
    State.Canvas.Font.Assign(UniFont[ftSmall]);
    NewStat;
    end;

  cReleaseModule:
    begin
    Shade.Free;
    State.Free
    end;

  cNewGame,cLoadGame:
    begin
    inc(Round);
    if Mode=Running then
      begin Invalidate; Update end
    else Show;
    G:=TNewGameData(Data);
    LogDlg.mSlot.Visible:=false;
    LogDlg.Host:=nil;
    ToldAlive:=G.RO[me].Alive;
    Active:=-1;
    end;

  cBreakGame:
    begin
    LogDlg.List.Clear;
    if Mode<>Running then
      begin
      if LogDlg.Visible then LogDlg.Close;
      Close;
      end
    end;

  cTurn,cResume,cContinue:
    begin
    me:=Player;
    if Active>=0 then
      begin ShowActive(Active,false); Active:=-1 end;
    ShowYear;
    if (G.RO[me].Alive<>ToldAlive) then
      begin
      for p:=1 to nPlOffered-1 do
        if 1 shl p and (G.RO[me].Alive xor ToldAlive)<>0 then
          begin
          r:=Rect(xBrain[p],yBrain[p],xBrain[p]+67,yBrain[p]+71);
          InvalidateRect(Handle,@r,false);
          end;
      ToldAlive:=G.RO[me].Alive;
      end;
    Application.ProcessMessages;
    if Mode=Quit then EndPlaying
    else if G.RO[me].Happened and phGameEnd<>0 then
      begin // game ended, update statistics
      for p:=1 to nPlOffered-1 do if bixView[p]>=0 then
        if 1 shl p and G.RO[me].Alive=0 then inc(ExtStat[p]) // extinct
        else if G.RO[me].Alive=1 shl p then inc(WinStat[p]) // only player alive
        else
          begin // alive but not alone -- check colony ship
          ShipComplete:=true;
          for i:=0 to nShipPart-1 do
            if G.RO[me].Ship[p].Parts[i]<ShipNeed[i] then
              ShipComplete:=false;
          if ShipComplete then inc(WinStat[p])
          end;
      if Mode=Running then Server(sNextRound,me,0,nil^)
      end
    else if Mode=Running then Server(sTurn,me,0,nil^);
    if Mode=Stop then
      begin
      GoBtn.ButtonIndex:=22;
      Mode:=Stopped
      end
    end;

  cShowTurnChange:
    begin
    if Active>=0 then
      begin ShowActive(Active,false); Active:=-1 end;
    Active:=integer(Data);
    ShowActive(Active,true);
    end

  end
end;

procedure TNoTermDlg.GoBtnClick(Sender: TObject);
begin
if Mode=Running then Mode:=Stop
else if Mode=Stopped then
  begin
  Mode:=Running;
  GoBtn.ButtonIndex:=23;
  GoBtn.Update;
  Server(sTurn,me,0,nil^);
  end
end;

procedure TNoTermDlg.QuitBtnClick(Sender: TObject);
begin
if Mode=Stopped then EndPlaying
else Mode:=Quit
end;

procedure TNoTermDlg.FormPaint(Sender: TObject);
var
i: integer;
begin
Fill(Canvas,3,3,ClientWidth-6, ClientHeight-6, 0,0);
Frame(Canvas,0,0,ClientWidth-1,ClientHeight-1, $000000,$000000);
Frame(Canvas,1,1,ClientWidth-2,ClientHeight-2,
  MainTexture.clBevelLight,MainTexture.clBevelShade);
Frame(Canvas,2,2,ClientWidth-3,ClientHeight-3,
  MainTexture.clBevelLight,MainTexture.clBevelShade);
Corner(Canvas,1,1,0,MainTexture);
Corner(Canvas,ClientWidth-9,1,1,MainTexture);
Corner(Canvas,1,ClientHeight-9,2,MainTexture);
Corner(Canvas,ClientWidth-9,ClientHeight-9,3,MainTexture);
Canvas.Font.Assign(UniFont[ftCaption]);
RisedTextOut(Canvas,(ClientWidth-BiColorTextWidth(Canvas,Caption)) div 2,7,Caption);
Canvas.Font.Assign(UniFont[ftSmall]);
for i:=1 to nPlOffered-1 do if bixView[i]>=0 then
  begin
  FrameImage(Canvas,StartDlg.BrainPicture[bixView[i]],xBrain[i],yBrain[i],64,64,0,0);
  if 1 shl i and G.RO[me].Alive=0 then
    BitBlt(Canvas.Handle,xBrain[i],yBrain[i],64,64,
      Shade.Canvas.Handle,0,0,SRCAND);
  with Canvas do
    begin
    DarkGradient(Canvas,xBrain[i]+4,yBrain[i]+53,56,1);
    Font.Color:=$3FBBDF;
    Textout(xBrain[i]+8,yBrain[i]+52,IntToStr(WinStat[i]));
    Font.Color:=$0000FF;
    Textout(xBrain[i]+34,yBrain[i]+52,IntToStr(ExtStat[i]));
    end;
  ShowActive(i, i=Active);
  end;
BitBlt(Canvas.Handle,x0Brain+32-20,y0Brain+32-20,40,40,
  GrExt[HGrSystem2].Mask.Canvas.Handle,115,1,SRCAND);
BitBlt(Canvas.Handle,x0Brain+32-20,y0Brain+32-20,40,40,
  GrExt[HGrSystem2].Data.Canvas.Handle,115,1,SRCPAINT);
ShowYear;
BtnFrame(Canvas,GoBtn.BoundsRect,MainTexture);
BtnFrame(Canvas,QuitBtn.BoundsRect,MainTexture);
//BtnFrame(Canvas,StatBtn.BoundsRect,MainTexture);
end;

procedure Client;
begin
if not FormsCreated then
  begin
  FormsCreated:=true;
  Application.CreateForm(TNoTermDlg, NoTermDlg);
  end;
NoTermDlg.Client(Command,Player,Data);
end;

procedure TNoTermDlg.FormKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
if (char(Key)='M') and (ssCtrl in Shift) then
  if LogDlg.Visible then LogDlg.Close else LogDlg.Show;
end;

initialization
FormsCreated:=false;

end.

