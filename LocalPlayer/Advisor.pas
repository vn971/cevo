{$INCLUDE switches}
unit Advisor;

interface

uses
  Messg,Protocol,

  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ButtonA,
  ButtonBase, ButtonB;

type
  TAdvisorDlg = class(TBaseMessgDlg)
    NextBtn: TButtonB;
    PrevBtn: TButtonB;
    OKBtn: TButtonA;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure NextBtnClick(Sender: TObject);
    procedure PrevBtnClick(Sender: TObject);

  public
    Icon: TBitmap;
    procedure Init;
    procedure DeInit;
    procedure NewGame(var Data);
    procedure BreakGame;
    procedure GiveStrategyAdvice;
    procedure GiveCityAdvice(cix: integer);
    function HaveStrategyAdvice: boolean;
    function HaveCityAdvice(cix: integer): boolean;

  private
    MaxLines,Page: integer;
    hm: integer; {module handle}
    AdvisorCall: TClientCall; {client function address}
    Advices: TStringList;
    Image: TBitmap;
    procedure MakeIcon;
    procedure AddAdvice(Text: string; Nation, CityID, Loc: integer);
    procedure Go;
    procedure ChangePage(NewPage: integer);
    procedure SmartInvalidate;
  end;

var
  AdvisorDlg: TAdvisorDlg;

implementation

uses
ClientTools,ScreenTools,Term,Tribes;

{$R *.DFM}

const
ImageDown=16;

procedure GiveAdvice(Text: PChar; Nation, CityID, Loc: integer); stdcall;
begin
AdvisorDlg.AddAdvice(Text, Nation, CityID, Loc);
end;

procedure TAdvisorDlg.FormCreate(Sender: TObject);
begin
inherited;
Advices:=TStringList.Create;
OKBtn.Caption:=Phrases.Lookup('BTN_OK');
end;

procedure TAdvisorDlg.FormDestroy(Sender: TObject);
begin
Advices.Free;
end;

procedure TAdvisorDlg.Init;
//var
//InitAdvisorData: TInitAdvisorData;
begin
(*Image:=TBitmap.Create;
LoadGraphicFile(Image, 'advisor');
Image.PixelFormat:=pf24bit;
MakeIcon;
hm:=LoadLibrary(PChar(HomeDir+'advisor kit\advisorproject.dll'));
AdvisorCall:=GetProcAddress(hm,'aclient');
InitAdvisorData.Server:=@Server;
InitAdvisorData.GiveAdvice:=@GiveAdvice;
AdvisorCall(aInitModule,0,InitAdvisorData);*)
end;

procedure TAdvisorDlg.DeInit;
begin
(*AdvisorCall(aReleaseModule,0,nil^);
FreeLibrary(hm);
Image.Free;
Icon.Free;*)
end;

procedure TAdvisorDlg.MakeIcon;
// resample image to 16x16
type
TLine=array[0..99999,0..2] of Byte;
var
x,y,ch: integer;
srcline,dstline: ^TLine;
begin
Icon:=TBitmap.Create;
Icon.PixelFormat:=pf24bit;
Icon.Canvas.Brush.Color:=$000000;
Icon.Width:=16; Icon.Height:=16;
for y:=0 to 63 do if y<Image.Height then
  begin
  srcline:=Image.ScanLine[y];
  dstline:=Icon.ScanLine[y div 4];
  for x:=0 to 63 do if x<Image.Width then
    for ch:=0 to 2 do
      dstline[x div 4][ch]:=dstline[x div 4][ch]+srcline[x][ch] shr 4;
  end;
end;

procedure TAdvisorDlg.NewGame(var Data);
begin
//AdvisorCall(aNewGame,0,Data);
end;

procedure TAdvisorDlg.BreakGame;
begin
//AdvisorCall(aBreakGame,0,nil^);
end;

procedure TAdvisorDlg.GiveStrategyAdvice;
begin
Advices.Clear;
AdvisorCall(aGiveStrategyAdvice,me,nil^);
Go;
end;

procedure TAdvisorDlg.GiveCityAdvice(cix: integer);
begin
Advices.Clear;
AdvisorCall(aGiveCityAdvice,me,cix);
Go;
end;

function TAdvisorDlg.HaveStrategyAdvice: boolean;
begin
result:=true; //!!!
end;

function TAdvisorDlg.HaveCityAdvice(cix: integer): boolean;
begin
result:=true; //!!!
end;

procedure TAdvisorDlg.AddAdvice(Text: string; Nation, CityID, Loc: integer);
begin
if CityID>=0 then
  Text:=Format(Text, [CityName(CityID)]);
if Nation>=0 then
  Text:=Tribe[Nation].TString(Text);
Advices.AddObject(Text, TObject(Loc));
end;

procedure TAdvisorDlg.Go;
begin
if Advices.Count>0 then
  ShowModal;
end;

procedure TAdvisorDlg.FormShow(Sender: TObject);
var
i: integer;
begin
MaxLines:=0;
for i:=0 to Advices.Count-1 do
  begin
  MessgText:=Advices[i];
  SplitText(true);
  if Lines>MaxLines then MaxLines:=Lines;
  end;
Lines:=MaxLines;
TopSpace:=80;
CorrectHeight;
Left:=8;
Top:=Screen.Height-PanelHeight-Height-8;
ChangePage(0);
end;

procedure TAdvisorDlg.FormPaint(Sender:TObject);
begin
inherited;
FrameImage(Canvas, Image, (ClientWidth-66) div 2,Border+ImageDown,64,64,0,0);
//Frame(Canvas,(ClientWidth-66) div 2-1,Border+16-1,(ClientWidth-66) div 2+64,Border+16+64,Tex.clBevelShade,Tex.clBevelLight);
//BitBlt(Canvas.Handle, (ClientWidth-66) div 2,Border+16,64,64,Image.Canvas.Handle,0,0,SRCCOPY);
end; {FormPaint}

procedure TAdvisorDlg.OKBtnClick(Sender: TObject);
begin
ModalResult:=mrOK;
end;

procedure TAdvisorDlg.NextBtnClick(Sender: TObject);
begin
ChangePage(Page+1);
SmartInvalidate;
end;

procedure TAdvisorDlg.PrevBtnClick(Sender: TObject);
begin
ChangePage(Page-1);
SmartInvalidate;
end;

procedure TAdvisorDlg.ChangePage(NewPage: integer);
begin
Page:=NewPage;
MessgText:=Advices[Page];
PrevBtn.Visible:= Page>0;
NextBtn.Visible:= Page<Advices.Count-1;
SplitText(true);
TopSpace:=80+(MaxLines-Lines)*MessageLineSpacing div 2;
MainScreen.SetAdviceLoc(integer(Advices.Objects[Page]), BoundsRect);
end;

procedure TAdvisorDlg.SmartInvalidate;
var
i: integer;
r0,r1: HRgn;
begin
r0:=CreateRectRgn(0,Border+ImageDown+65,ClientWidth,ClientHeight);
for i:=0 to ControlCount-1 do if Controls[i].Visible then
  begin
  with Controls[i].BoundsRect do
    r1:=CreateRectRgn(Left,Top,Right,Bottom);
  CombineRgn(r0,r0,r1,RGN_DIFF);
  DeleteObject(r1);
  end;
InvalidateRgn(Handle,r0,false);
DeleteObject(r0);
end;

end.

