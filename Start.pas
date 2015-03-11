{$INCLUDE switches}

unit Start;

interface

uses
  GameServer,ButtonBase,

  Windows,Messages,SysUtils,Classes,Graphics,Controls,Forms,StdCtrls,
  Menus,Registry,ButtonA,ButtonC, ButtonB;

type
  TStartDlg = class(TForm)
    PopupMenu1: TPopupMenu;
    StartBtn: TButtonA;
    Down1Btn: TButtonC;
    Up1Btn: TButtonC;
    List: TListBox;
    RenameBtn: TButtonB;
    DeleteBtn: TButtonB;
    Down2Btn: TButtonC;
    Up2Btn: TButtonC;
    QuitBtn: TButtonB;
    HelpBtn: TButtonB;
    CustomizeBtn: TButtonC;
    AutoDiffUpBtn: TButtonC;
    AutoDiffDownBtn: TButtonC;
    AutoEnemyUpBtn: TButtonC;
    AutoEnemyDownBtn: TButtonC;
    ReplayBtn: TButtonB;
    procedure StartBtnClick(Sender:TObject);
    procedure FormPaint(Sender:TObject);
    procedure FormShow(Sender:TObject);
    procedure FormDestroy(Sender:TObject);
    procedure FormCreate(Sender:TObject);
    procedure BrainClick(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: integer);
    procedure Up1BtnClick(Sender: TObject);
    procedure Down1BtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListClick(Sender: TObject);
    procedure RenameBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure DiffBtnClick(Sender: TObject);
    procedure MultiBtnClick(Sender: TObject);
    procedure Up2BtnClick(Sender: TObject);
    procedure Down2BtnClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure QuitBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CustomizeBtnClick(Sender: TObject);
    procedure AutoDiffUpBtnClick(Sender: TObject);
    procedure AutoDiffDownBtnClick(Sender: TObject);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure AutoEnemyUpBtnClick(Sender: TObject);
    procedure AutoEnemyDownBtnClick(Sender: TObject);
    procedure ReplayBtnClick(Sender: TObject);
  public
    BrainPicture: array[0..maxBrain-1] of TBitmap;
    EmptyPicture: TBitmap;
    procedure UpdateFormerGames;
    procedure UpdateMaps;
  private
    WorldSize, StartLandMass, MaxTurn, AutoEnemies, AutoDiff, MultiControl,
    MiniWidth, MiniHeight,
    Page, ShowTab, Tab, Diff0, bixDefault,
    nMapStartPositions,
    LoadTurn, LastTurn, {last turn of selected former game}
    SlotAvailable,
    bixPopup: integer; {brain concerned by brain context menu}
    ListIndex: array[0..2] of integer;
    MapFileName: string;
    FormerGames, Maps: TStringList;
    Mini:TBitmap; {game world sample preview}
    MiniColors: array[0..11,0..1] of TColor;
//    BookDate: string;
    DiffUpBtn: array[0..8] of TButtonC;
    DiffDownBtn: array[0..8] of TButtonC;
    MultiBtn: array[6..8] of TButtonC;
    MiniMode: (mmNone,mmPicture,mmMultiPlayer);
    Welcomed, TurnValid, Tracking: boolean;
    procedure InitPopup(PopupIndex: integer);
    procedure PaintInfo;
    procedure ChangePage(NewPage: integer);
    procedure ChangeTab(NewTab: integer);
    procedure UnlistBackupFile(FileName: string);
    procedure OnEraseBkgnd(var m:TMessage); message WM_ERASEBKGND;
    procedure SmartInvalidate(x0,y0,x1,y1: integer);
  end;

var
  StartDlg:TStartDlg;

implementation

uses Directories, Protocol, Direct, ScreenTools, Inp, Messg;

{$R *.DFM}

const
// predefined world size
// attention: lx*ly+1 must be prime!
{nWorldSize=8;
lxpre: array[0..nWorldSize-1] of integer =(30,40,50,60,70,90,110,130);
lypre: array[0..nWorldSize-1] of integer =(46,52,60,70,84,94,110,130);
DefaultWorldTiles=4200;}
nWorldSize=6;
lxpre: array[0..nWorldSize-1] of integer =(30,40,50,60,75,100);
lypre: array[0..nWorldSize-1] of integer =(46,52,60,70,82,96);
DefaultWorldTiles=4150;
DefaultWorldSize=3;
DefaultLandMass=30;

nPlOffered=9;
x0Mini=437; y0Mini=164;
xTurnSlider=346; yTurnSlider=248; wTurnSlider=168;
yLogo=60;
xDefault=234; yDefault=134;
x0Brain=146; y0Brain=134;
dxBrain=104; dyBrain=80;
xBrain: array[0..nPlOffered-1] of integer =
  (x0Brain,x0Brain,x0Brain+dxBrain,x0Brain+dxBrain,x0Brain+dxBrain,x0Brain,
  x0Brain-dxBrain,x0Brain-dxBrain,x0Brain-dxBrain);
yBrain: array[0..nPlOffered-1] of integer =
  (y0Brain,y0Brain-dyBrain,y0Brain-dyBrain,y0Brain,y0Brain+dyBrain,
  y0Brain+dyBrain,y0Brain+dyBrain,y0Brain,y0Brain-dyBrain);
TabSize=164; TabHeight=28; QuitTabSize=0{104};

MaxWidthMapLogo=96; MaxHeightMapLogo=96;

InitAlive: array[1..nPl] of integer=
(1,1+2,1+2+32,1+2+8+128,1+2+8+32+128,1+2+8+16+64+128,1+2+4+16+32+64+256,
511-32,511,511-32,511,511-32,511,511-32,511);
InitMulti: array[nPlOffered+1..nPl] of integer=
(256,256,256+128,256+128,256+128+64,256+128+64);

pgStartRandom=0; pgStartMap=1; pgNoLoad=2; pgLoad=3; pgEditRandom=4;
pgEditMap=5;

OfferMultiple=[6,7,8];

PlayerAutoDiff: array[1..5] of integer=(1,2,2,2,3);
EnemyAutoDiff: array[1..5] of integer=(3,3,2,1,1);


procedure TStartDlg.FormCreate(Sender:TObject);
var
x,y,i: integer;
DefaultAI,s: string;
r0,r1: HRgn;
Reg: TRegistry;
FirstStart: boolean;
begin
Left:=(Screen.Width-800)*3 div 8;
Top:=Screen.Height-ClientHeight-(Screen.Height-600) div 3;

Reg:=TRegistry.Create;
FirstStart:=not Reg.KeyExists('SOFTWARE\cevo\RegVer8\Start');
Welcomed:=not FirstStart;

if FirstStart then
  begin
  // initialize AI assignment
  Reg.OpenKey('SOFTWARE\cevo\RegVer8\Start',true);
  for i:=0 to nPlOffered-1 do
    begin
    if i=0 then s:=':StdIntf'
    else s:='StdAI';
    Reg.WriteString('Control'+IntToStr(i),s);
    Reg.WriteInteger('Diff'+IntToStr(i),2);
    end;
  Reg.WriteInteger('MultiControl',0);
  Reg.closekey;

  // register file type: "cevo Book" -- fails with no administrator rights!
  try
    Reg.RootKey:=HKEY_CLASSES_ROOT;
    Reg.OpenKey ('.cevo',true);
    Reg.WriteString ('','cevoBook');
    Reg.closekey;
    Reg.OpenKey ('cevoBook',true);
    Reg.WriteString ('','cevo Book');
    Reg.closekey;
    Reg.OpenKey ('cevoBook\DefaultIcon',true);
    Reg.WriteString ('',ParamStr(0)+',0');
    Reg.closekey;
    Reg.OpenKey ('cevoBook\shell\open\command',true);
    Reg.WriteString ('',ParamStr(0)+' "%1"');
    Reg.closekey;
  except
    end;
  end
else
  begin
  Reg.OpenKey('SOFTWARE\cevo\RegVer8\Start',false);
  try
    WorldSize:=Reg.ReadInteger('WorldSize');
    StartLandMass:=Reg.ReadInteger('LandMass');
    MaxTurn:=Reg.ReadInteger('MaxTurn');
    DefaultAI:=Reg.ReadString('DefaultAI');
    AutoEnemies:=Reg.ReadInteger('AutoEnemies');
    AutoDiff:=Reg.ReadInteger('AutoDiff');
  except
    FirstStart:=true;
    end;
  Reg.closekey;
  end;
if FirstStart then
  begin
  WorldSize:=DefaultWorldSize;
  StartLandMass:=DefaultLandMass;
  MaxTurn:=800;
  DefaultAI:='StdAI';
  AutoEnemies:=8;
  AutoDiff:=1;
  end;
Reg.Free;

bixDefault:=-1;
for i:=bixRandom to nBrain-1 do
  if AnsiCompareFileName(DefaultAI,Brain[i].FileName)=0 then bixDefault:=i;
if (bixDefault=bixRandom) and (nBrain<bixFirstAI+2) then
  bixDefault:=-1;
if (bixDefault<0) and (nBrain>bixFirstAI) then bixDefault:=bixFirstAI; // default AI not found, use any

r0:=CreateRectRgn(0,0,ClientWidth,ClientHeight);
r1:=CreateRectRgn(3*TabSize+2,0,ClientWidth-QuitTabSize,TabHeight);
CombineRgn(r0,r0,r1,RGN_DIFF);
//DeleteObject(r1);
r1:=CreateRectRgn(QuitBtn.left,QuitBtn.Top,QuitBtn.left+QuitBtn.Width,
  QuitBtn.Top+QuitBtn.Height);
CombineRgn(r0,r0,r1,RGN_OR);
//DeleteObject(r1);
r1:=CreateRectRgn(HelpBtn.left,HelpBtn.Top,HelpBtn.left+HelpBtn.Width,
  HelpBtn.Top+HelpBtn.Height);
CombineRgn(r0,r0,r1,RGN_OR);
//DeleteObject(r1);
SetWindowRgn(Handle,r0,false);
//DeleteObject(r0); // causes crash with Windows 95

Canvas.Font.Assign(UniFont[ftNormal]);
Canvas.Brush.Style:=bsClear;

QuitBtn.Hint:=Phrases.Lookup('STARTCONTROLS',0);
HelpBtn.Hint:=Phrases.Lookup('STARTCONTROLS',13);
ReplayBtn.Hint:=Phrases.Lookup('BTN_REPLAY');
for i:=0 to nPlOffered-1 do
  begin
  DiffUpBtn[i]:=TButtonC.Create(self);
  DiffUpBtn[i].Graphic:=GrExt[HGrSystem].Data;
  DiffUpBtn[i].Left:=xBrain[i]-18;
  DiffUpBtn[i].Top:=yBrain[i]+39;
  DiffUpBtn[i].ButtonIndex:=1;
  DiffUpBtn[i].Parent:=self;
  DiffUpBtn[i].OnClick:=DiffBtnClick;
  DiffDownBtn[i]:=TButtonC.Create(self);
  DiffDownBtn[i].Graphic:=GrExt[HGrSystem].Data;
  DiffDownBtn[i].Left:=xBrain[i]-18;
  DiffDownBtn[i].Top:=yBrain[i]+51;
  DiffDownBtn[i].ButtonIndex:=0;
  DiffDownBtn[i].Parent:=self;
  DiffDownBtn[i].OnClick:=DiffBtnClick;
  end;
for i:=6 to 8 do
  begin
  MultiBtn[i]:=TButtonC.Create(self);
  MultiBtn[i].Graphic:=GrExt[HGrSystem].Data;
  MultiBtn[i].Left:=xBrain[i]-18;
  MultiBtn[i].Top:=yBrain[i];
  MultiBtn[i].Parent:=self;
  MultiBtn[i].OnClick:=MultiBtnClick;
  end;

x:=BiColorTextWidth(Canvas,Phrases.Lookup('STARTCONTROLS',7)) div 2;
CustomizeBtn.Left:=x0Brain+32-16-x;
if AutoDiff<0 then CustomizeBtn.ButtonIndex:=3
else CustomizeBtn.ButtonIndex:=2;

BrainPicture[0]:=TBitmap.Create;
BrainPicture[0].Width:=64; BrainPicture[0].Height:=64;
BitBlt(BrainPicture[0].Canvas.Handle,0,0,64,64,
  GrExt[HGrSystem2].Data.Canvas.Handle,1,111,SRCCOPY);
BrainPicture[1]:=TBitmap.Create;
BrainPicture[1].Width:=64; BrainPicture[1].Height:=64;
BitBlt(BrainPicture[1].Canvas.Handle,0,0,64,64,
  GrExt[HGrSystem2].Data.Canvas.Handle,66,111,SRCCOPY);
BrainPicture[2]:=TBitmap.Create;
BrainPicture[2].Width:=64; BrainPicture[2].Height:=64;
BitBlt(BrainPicture[2].Canvas.Handle,0,0,64,64,
  GrExt[HGrSystem2].Data.Canvas.Handle,131,111,SRCCOPY);
BrainPicture[3]:=TBitmap.Create;
BrainPicture[3].Width:=64; BrainPicture[3].Height:=64;
BitBlt(BrainPicture[3].Canvas.Handle,0,0,64,64,
  GrExt[HGrSystem2].Data.Canvas.Handle,131,46,SRCCOPY);
for i:=bixFirstAI to nBrain-1 do
  begin
  BrainPicture[i]:=TBitmap.Create;
  if not LoadGraphicFile(BrainPicture[i], Brain[i].FileName, gfNoError) then
    begin
    BrainPicture[i].Width:=64; BrainPicture[i].Height:=64;
    with BrainPicture[i].Canvas do
      begin
      Brush.Color:=$904830;
      FillRect(Rect(0,0,64,64));
      Font.Assign(UniFont[ftTiny]);
      Font.Style:=[];
      Font.Color:=$5FDBFF;
      Textout(32-TextWidth(Brain[i].FileName) div 2,
        32-TextHeight(Brain[i].FileName) div 2,Brain[i].FileName);
      end
    end
  end;

EmptyPicture:=TBitmap.Create;
EmptyPicture.PixelFormat:=pf24bit;
EmptyPicture.Width:=64; EmptyPicture.Height:=64;

Mini:=TBitmap.Create;
for x:=0 to 11 do for y:=0 to 1 do
  MiniColors[x,y]:=GrExt[HGrSystem].Data.Canvas.Pixels[66+x,67+y];
InitButtons(self);

bixView[0]:=bixTerm;
SlotAvailable:=-1;
Tab:=1;
ShowTab:=1;
Diff0:=2;
TurnValid:=false;
Tracking:=false;
FormerGames:=TStringList.Create;
UpdateFormerGames;
ShowTab:=1; // always start with new book page
MapFileName:='';
Maps:=TStringList.Create;
UpdateMaps;
end;

procedure TStartDlg.FormDestroy(Sender:TObject);
var
i: integer;
begin
FormerGames.Free;
Maps.Free;
Mini.Free;
EmptyPicture.Free;
for i:=0 to nBrain-1 do
  BrainPicture[i].Free;
end;

procedure TStartDlg.OnEraseBkgnd(var m:TMessage);
begin
end;

procedure TStartDlg.SmartInvalidate(x0,y0,x1,y1: integer);
var
i: integer;
r0,r1: HRgn;
begin
r0:=CreateRectRgn(x0,y0,x1,y1);
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

procedure TStartDlg.FormPaint(Sender:TObject);
const
TabNames: array[0..2] of integer=(11,3,4);
var
i,w,xMini,yMini:integer;
s: string;
begin
PaintBackground(self,3,3,3*TabSize-4,TabHeight-3);
PaintBackground(self,3,TabHeight+3,ClientWidth-6,ClientHeight-TabHeight-6);
if Page in [pgStartRandom,pgStartMap] then
  begin
  Frame(Canvas,328,112-15,ClientWidth,Up2Btn.Top+38,
    MainTexture.clBevelShade,MainTexture.clBevelLight);
  if AutoDiff>0 then
    begin
    Frame(Canvas,-1{x0Brain-dxBrain},112-15{Up1Btn.Top-12}{y0Brain-dyBrain},x0Brain+dxBrain+64,
(*      120+64+12{Up2Btn.Top-28}{y0Brain+dyBrain+64}, MainTexture.clBevelShade,MainTexture.clBevelLight);
    Frame(Canvas,-1{x0Brain-dxBrain},Up2Btn.Top-12{y0Brain-dyBrain},x0Brain+dxBrain+64,
*)      Up2Btn.Top+38{y0Brain+dyBrain+64}, MainTexture.clBevelShade,MainTexture.clBevelLight);
    end
  end
else Frame(Canvas,328,Up1Btn.Top-15,ClientWidth,Up2Btn.Top+38,
  MainTexture.clBevelShade,MainTexture.clBevelLight);
Frame(Canvas,0,0,ClientWidth-1,ClientHeight-1,0,0);
for i:=0 to 2 do
  begin
  Frame(Canvas,i*TabSize+2,2,(i+1)*TabSize-1,TabHeight,MainTexture.clBevelLight,MainTexture.clBevelShade);
  Frame(Canvas,i*TabSize+1,1,(i+1)*TabSize,TabHeight,MainTexture.clBevelLight,MainTexture.clBevelShade);
  Canvas.Pixels[i*TabSize+1,1]:=MainTexture.clBevelShade;
  end;
Canvas.Font.Assign(UniFont[ftCaption]);
RisedTextOut(Canvas,13+Tab*TabSize,4,Phrases.Lookup('STARTCONTROLS',TabNames[Tab]));
Canvas.Font.Assign(UniFont[ftNormal]);
for i:=0 to 2 do if i<>Tab then
  RisedTextOut(Canvas,13+i*TabSize,5,Phrases.Lookup('STARTCONTROLS',TabNames[i]));
//Frame(Canvas,ClientWidth-QuitTabSize+1,1,ClientWidth-2,TabHeight,MainTexture.clBevelLight,MainTexture.clBevelShade);
//Frame(Canvas,ClientWidth-QuitTabSize+2,2,ClientWidth-3,TabHeight-1,MainTexture.clBevelLight,MainTexture.clBevelShade);
//Canvas.Pixels[ClientWidth-QuitTabSize+1,1]:=MainTexture.clBevelShade;
Frame(Canvas,3*TabSize+1,-1,ClientWidth-QuitTabSize,TabHeight,$000000,$000000);
Frame(Canvas,1,TabHeight+1,ClientWidth-2,ClientHeight-2,MainTexture.clBevelLight,
  MainTexture.clBevelShade);
Frame(Canvas,2,TabHeight+2,ClientWidth-3,ClientHeight-3,MainTexture.clBevelLight,
  MainTexture.clBevelShade);
PaintBackground(self,3+Tab*TabSize,TabHeight-1,TabSize-4,4);
Canvas.Pixels[Tab*TabSize+2,TabHeight]:=MainTexture.clBevelLight;
Canvas.Pixels[(Tab+1)*TabSize-1,TabHeight+1]:=MainTexture.clBevelShade;
if Tab<2 then
  Frame(Canvas,(Tab+1)*TabSize+1,3,(Tab+1)*TabSize+2,TabHeight,
    MainTexture.clBevelShade,MainTexture.clBevelShade); // tab shadow
//RisedTextOut(Canvas,ClientWidth-QuitTabSize+12,5,Phrases.Lookup('STARTCONTROLS',0));

if Page in [pgStartRandom,pgStartMap] then
  begin
  DLine(Canvas,344,514,y0Mini+61+19,MainTexture.clBevelLight,MainTexture.clBevelShade);
  RisedTextOut(Canvas,344,y0Mini+61,Phrases.Lookup('STARTCONTROLS',10));
  s:=TurnToString(MaxTurn);
  RisedTextOut(Canvas,514-BiColorTextWidth(Canvas,s),y0Mini+61,s);
  s:=Phrases.Lookup('STARTCONTROLS',7);
  w:=Canvas.TextWidth(s);
  Canvas.Font.Color:=MainTexture.clBevelLight;
  Canvas.TextOut(x0Brain+32+1-w div 2,y0Brain+dyBrain+69+1,s);
  Canvas.Font.Color:=MainTexture.clBevelShade;
  Canvas.TextOut(x0Brain+32-w div 2,y0Brain+dyBrain+69,s);

  InitOrnament;
  if AutoDiff<0 then
    begin
    for i:=12 to 19 do if (i<13) or (i>17) then
      begin
      BitBlt(Canvas.Handle,9+i*27,yLogo-2,wOrna,hOrna,
        GrExt[HGrSystem2].Mask.Canvas.Handle,xOrna,yOrna,SRCAND);
      BitBlt(Canvas.Handle,9+i*27,yLogo-2,wOrna,hOrna,
        GrExt[HGrSystem2].Data.Canvas.Handle,xOrna,yOrna,SRCPAINT);
      end;
    PaintLogo(Canvas,69+11*27,yLogo,MainTexture.clBevelLight,MainTexture.clBevelShade);

    for i:=0 to nPlOffered-1 do if 1 shl i and SlotAvailable<>0 then
      begin
      if bixView[i]>=0 then
        FrameImage(Canvas,BrainPicture[bixView[i]],xBrain[i],yBrain[i],64,64,0,0,true)
      else FrameImage(Canvas,EmptyPicture,xBrain[i],yBrain[i],64,64,0,0,true);
      if bixView[i]>=bixTerm then
        begin
        BitBlt(Canvas.Handle,xBrain[i]-18,yBrain[i]+19,12,14,
          GrExt[HGrSystem].Data.Canvas.Handle,134+(Difficulty[i]-1)*13,28,SRCCOPY);
        Frame(Canvas,xBrain[i]-19,yBrain[i]+18,xBrain[i]-18+12,yBrain[i]+(19+14),
          $000000,$000000);
        RFrame(Canvas,DiffUpBtn[i].Left-1,DiffUpBtn[i].Top-1,DiffUpBtn[i].Left+12,
          DiffUpBtn[i].Top+24,MainTexture.clBevelShade,MainTexture.clBevelLight);
        with Canvas do
          begin
          Brush.Color:=$000000;
          FillRect(Rect(xBrain[i]-5,yBrain[i]+25,xBrain[i]-2,yBrain[i]+27));
          Brush.Style:=bsClear;
          end;
        if i in OfferMultiple then
          begin
          RFrame(Canvas,MultiBtn[i].Left-1,MultiBtn[i].Top-1,MultiBtn[i].Left+12,
            MultiBtn[i].Top+12,MainTexture.clBevelShade,MainTexture.clBevelLight);
          BitBlt(Canvas.Handle,xBrain[i]-31,yBrain[i],13,12,
            GrExt[HGrSystem].Data.Canvas.Handle,88,47,SRCCOPY);
          end
        end;
      if bixView[i]>=0 then
        begin
        DiffUpBtn[i].Hint:=Format(Phrases.Lookup('STARTCONTROLS',9),
          [Brain[bixView[i]].Name]);
        DiffDownBtn[i].Hint:=DiffUpBtn[i].Hint;
        end
      end;
    end
  else
    begin
    DLine(Canvas,24,198,140+19,MainTexture.clBevelLight,MainTexture.clBevelShade);
    RisedTextOut(Canvas,24{x0Brain+32-BiColorTextWidth(Canvas,s) div 2},140{y0Mini-77},
      Phrases.Lookup('STARTCONTROLS',15));
    if Page=pgStartRandom then s:=IntToStr(AutoEnemies)
    else if nMapStartPositions=0 then s:='0'
    else s:=IntToStr(nMapStartPositions-1);
    RisedTextOut(Canvas,198-BiColorTextWidth(Canvas,s),140,s);
    DLine(Canvas,24,xDefault-6,164+19,MainTexture.clBevelLight,MainTexture.clBevelShade);
    RisedTextOut(Canvas,24{x0Brain+32-BiColorTextWidth(Canvas,s) div 2},164{y0Mini-77},
      Phrases.Lookup('STARTCONTROLS',16));
    FrameImage(Canvas,BrainPicture[bixDefault],xDefault,yDefault,64,64,0,0,true);
    DLine(Canvas,56,272,y0Mini+61+19,MainTexture.clBevelLight,MainTexture.clBevelShade);
    RisedTextOut(Canvas,56,y0Mini+61,Phrases.Lookup('STARTCONTROLS',14));
    s:=Phrases.Lookup('AUTODIFF',AutoDiff-1);
    RisedTextOut(Canvas,272-BiColorTextWidth(Canvas,s),y0Mini+61,s);

    for i:=0 to 19 do if (i<2) or (i>6) then
      begin
      BitBlt(Canvas.Handle,9+i*27,yLogo-2,wOrna,hOrna,
        GrExt[HGrSystem2].Mask.Canvas.Handle,xOrna,yOrna,SRCAND);
      BitBlt(Canvas.Handle,9+i*27,yLogo-2,wOrna,hOrna,
        GrExt[HGrSystem2].Data.Canvas.Handle,xOrna,yOrna,SRCPAINT);
      end;
    PaintLogo(Canvas,69,yLogo,MainTexture.clBevelLight,MainTexture.clBevelShade);
    end
  end
else if Page=pgLoad then
  begin
//  RisedTextOut(Canvas,x0Mini+2-BiColorTextWidth(Canvas,BookDate) div 2,y0Mini-73,BookDate);
  if LastTurn>0 then
    begin
    PaintProgressBar(canvas,6,xTurnSlider,yTurnSlider,0,
      LoadTurn*wTurnSlider div LastTurn,wTurnSlider,MainTexture);
    Frame(canvas,xTurnSlider-2,yTurnSlider-2,xTurnSlider+wTurnSlider+1,
      yTurnSlider+8,$B0B0B0,$FFFFFF);
    RFrame(canvas,xTurnSlider-3,yTurnSlider-3,xTurnSlider+wTurnSlider+2,
      yTurnSlider+9,$FFFFFF,$B0B0B0);
    end
  else DLine(Canvas,344,514,y0Mini+61+19,MainTexture.clBevelLight,MainTexture.clBevelShade);
  RisedTextOut(Canvas,344,y0Mini+61,Phrases.Lookup('STARTCONTROLS',8));
  s:=TurnToString(LoadTurn);
  RisedTextOut(Canvas,514-BiColorTextWidth(Canvas,s),y0Mini+61,s);
  end
else if Page=pgEditRandom then
  begin
  DLine(Canvas,344,514,y0Mini-77+19,MainTexture.clBevelLight,MainTexture.clBevelShade);
  RisedTextOut(Canvas,344,y0Mini-77,Phrases.Lookup('STARTCONTROLS',5));
  s:=IntToStr((lxpre[WorldSize]*lypre[WorldSize]*20 + DefaultWorldTiles div 2)
    div DefaultWorldTiles *5)+'%';
  RisedTextOut(Canvas,514-BiColorTextWidth(Canvas,s),y0Mini-77,s);
  DLine(Canvas,344,514,y0Mini+61+19,MainTexture.clBevelLight,MainTexture.clBevelShade);
  RisedTextOut(Canvas,344,y0Mini+61,Phrases.Lookup('STARTCONTROLS',6));
  s:=IntToStr(StartLandMass)+'%';
  RisedTextOut(Canvas,514-BiColorTextWidth(Canvas,s),y0Mini+61,s);
  end;

if StartBtn.Visible then
  BtnFrame(Canvas,StartBtn.BoundsRect,MainTexture);
if Up2Btn.Visible then
  RFrame(Canvas,Up2Btn.Left-1,Up2Btn.Top-1,Up2Btn.Left+12,
    Up2Btn.Top+24,MainTexture.clBevelShade,MainTexture.clBevelLight);
if Up1Btn.Visible then
  RFrame(Canvas,Up1Btn.Left-1,Up1Btn.Top-1,Up1Btn.Left+12,
    Up1Btn.Top+24,MainTexture.clBevelShade,MainTexture.clBevelLight);
if AutoDiffUpBtn.Visible then
  RFrame(Canvas,AutoDiffUpBtn.Left-1,AutoDiffUpBtn.Top-1,AutoDiffUpBtn.Left+12,
    AutoDiffUpBtn.Top+24,MainTexture.clBevelShade,MainTexture.clBevelLight);
if AutoEnemyUpBtn.Visible then
  RFrame(Canvas,AutoEnemyUpBtn.Left-1,AutoEnemyUpBtn.Top-1,AutoEnemyUpBtn.Left+12,
    AutoEnemyUpBtn.Top+24,MainTexture.clBevelShade,MainTexture.clBevelLight);
if CustomizeBtn.Visible then
  RFrame(Canvas,CustomizeBtn.Left-1,CustomizeBtn.Top-1,CustomizeBtn.Left+12,
    CustomizeBtn.Top+12,MainTexture.clBevelShade,MainTexture.clBevelLight);
if List.Visible then
  EditFrame(Canvas,List.BoundsRect,MainTexture);
if RenameBtn.Visible then
  BtnFrame(Canvas,RenameBtn.BoundsRect,MainTexture);
if DeleteBtn.Visible then
  BtnFrame(Canvas,DeleteBtn.BoundsRect,MainTexture);
if Page=pgLoad then
  BtnFrame(Canvas,ReplayBtn.BoundsRect,MainTexture);

if Page<>pgNoLoad then
  begin
  xMini:=x0Mini-MiniWidth;
  yMini:=y0Mini-MiniHeight div 2;
  Frame(Canvas,xMini,yMini,xMini+3+MiniWidth*2,yMini+3+MiniHeight,MainTexture.clBevelLight,
    MainTexture.clBevelShade);
  Frame(Canvas,xMini+1,yMini+1,xMini+2+MiniWidth*2,yMini+2+MiniHeight,MainTexture.clBevelShade,
    MainTexture.clBevelLight);
  end;
s:='';
if MiniMode=mmPicture then
  begin
  BitBlt(Canvas.Handle,xMini+2,yMini+2,MiniWidth*2,MiniHeight,Mini.Canvas.Handle,0,0,SRCCOPY);
  if page=pgStartRandom then s:=Phrases.Lookup('RANMAP')
  end
else if MiniMode=mmMultiPlayer then s:=Phrases.Lookup('MPMAP')
else if page=pgStartMap then s:=Copy(MapFileName,1,Length(MapFileName)-9)
else if page=pgEditMap then s:=List.Items[List.ItemIndex]
else if page=pgNoLoad then s:=Phrases.Lookup('NOGAMES');
if s<>'' then
  RisedTextOut(Canvas,x0Mini+2-BiColorTextWidth(Canvas,s) div 2,y0Mini-8,s);
end;

procedure TStartDlg.FormShow(Sender:TObject);
type
TLine=array[0..99999999] of Byte;
var
i,x,y: integer;
PictureLine: ^TLine;
begin
GenerateNames:=true;

SetMainTextureByAge(-1);
List.Font.Color:=MainTexture.clMark;
Fill(EmptyPicture.Canvas,0,0,64,64,(wMaintexture-64) div 2,
  (hMaintexture-64) div 2);
for y:=0 to 63 do
  begin // darken texture for empty slot
  PictureLine:=EmptyPicture.ScanLine[y];
  for x:=0 to 64*3-1 do
    begin
    i:=integer(PictureLine[x])-28;
    if i<0 then i:=0;
    PictureLine[x]:=i;
    end
  end;

Difficulty[0]:=Diff0;

if not Welcomed then
  begin
  // no welcome message currently
  Welcomed:=true;
  end;
if ShowTab=2 then PreviewMap(StartLandMass); // avoid delay on first tab change 
ChangeTab(ShowTab);
end;

procedure TStartDlg.UnlistBackupFile(FileName: string);
var
i: integer;
begin
if FileName[1]<>'~' then FileName:='~'+FileName;
i:=FormerGames.Count-1;
while (i>=0) and (AnsiCompareFileName(FormerGames[i],FileName)<>0) do dec(i);
if i>=0 then
  begin
  FormerGames.Delete(i);
  if ListIndex[2]=i then ListIndex[2]:=0
  end
end;

procedure TStartDlg.StartBtnClick(Sender:TObject);
var
i,GameCount,MapCount: integer;
FileName: string;
Reg: TRegistry;
begin
case Page of
  pgLoad:
    begin //load
    FileName:=List.Items[List.ItemIndex];
    if LoadGame(DataDir+'Saved\', FileName+'.cevo', LoadTurn,false) then
      UnlistBackupFile(FileName)
    else SimpleMessage(Phrases.Lookup('LOADERR'));
    SlotAvailable:=-1;
    end;

  pgStartRandom,pgStartMap: if bixView[0]>=0 then
    begin
    if (page=pgStartMap) and (nMapStartPositions=0) and (AutoDiff>0) then
      begin
      SimpleMessage(Phrases.Lookup('NOSTARTPOS'));
      exit
      end;

    Reg:=TRegistry.Create;
    Reg.OpenKey('SOFTWARE\cevo\RegVer8\Start',true);
    try
      GameCount:=Reg.ReadInteger('GameCount');
    except
      GameCount:=0;
      end;

    if (AutoDiff<0) and (bixView[0]=bixNoTerm) then FileName:='Round'
    else
      begin
      inc(GameCount);
      FileName:=Format(Phrases.Lookup('GAME'),[GameCount]);
      end;

    // save settings and AI assignment
    if page=pgStartRandom then
      begin
      Reg.WriteInteger('WorldSize',WorldSize);
      Reg.WriteInteger('LandMass',StartLandMass);
      if AutoDiff<0 then
        for i:=0 to nPlOffered-1 do
          begin
          if bixView[i]=-1 then Reg.WriteString('Control'+IntToStr(i),'')
          else Reg.WriteString('Control'+IntToStr(i),Brain[bixView[i]].FileName);
          Reg.WriteInteger('Diff'+IntToStr(i),Difficulty[i]);
          end;
      Reg.WriteInteger('MultiControl',MultiControl);
      end;

    if AutoDiff>0 then
      begin
      Reg.WriteString('DefaultAI',Brain[bixDefault].FileName);
      SlotAvailable:=0; // bixView will be invalid hereafter
      bixView[0]:=bixTerm;
      Difficulty[0]:=PlayerAutoDiff[AutoDiff];
      for i:=1 to nPl-1 do
        if (Page=pgStartRandom) and (i<=AutoEnemies)
          or (Page=pgStartMap) and (i<nMapStartPositions) then
          begin
          bixView[i]:=bixDefault;
          Difficulty[i]:=EnemyAutoDiff[AutoDiff];
          end
        else bixView[i]:=-1;
      end
    else
      begin
      for i:=6 to 8 do
        if (bixView[0]<>bixNoTerm) and (MultiControl and (1 shl i)<>0) then
          begin
          bixView[i+3]:=bixView[i];
          Difficulty[i+3]:=Difficulty[i];
          bixView[i+6]:=bixView[i];
          Difficulty[i+6]:=Difficulty[i];
          end
        else
          begin
          bixView[i+3]:=-1;
          bixView[i+6]:=-1;
          end
      end;

    Reg.WriteInteger('AutoDiff',AutoDiff);
    Reg.WriteInteger('AutoEnemies',AutoEnemies);
    Reg.WriteInteger('MaxTurn',MaxTurn);
    Reg.WriteInteger('GameCount',GameCount);
    Reg.closekey;
    Reg.Free;

    StartNewGame(DataDir+'Saved\', FileName+'.cevo', MapFileName,
      lxpre[WorldSize], lypre[WorldSize], StartLandMass, MaxTurn);
    UnlistBackupFile(FileName);
    end;

  pgEditMap: EditMap(MapFileName, lxmax, lymax, StartLandMass);

  pgEditRandom: // new map
    begin
    Reg:=TRegistry.Create;
    Reg.OpenKey('SOFTWARE\cevo\RegVer8\Start',true);
    try
      MapCount:=Reg.ReadInteger('MapCount');
    except
      MapCount:=0;
      end;
    inc(MapCount);
    Reg.WriteInteger('MapCount',MapCount);
    Reg.closekey;
    Reg.Free;
    MapFileName:=Format(Phrases.Lookup('MAP'),[MapCount])+'.cevo map';
    EditMap(MapFileName, lxpre[WorldSize], lypre[WorldSize], StartLandMass);
    end
  end
end;

procedure TStartDlg.PaintInfo;

  procedure PaintRandomMini(Brightness: integer);
  type
  TLine=array[0..lxmax*2,0..2] of Byte;
  var
  i,x,y,xm,cm:integer;
  MiniLine:^TLine;
  Map: ^TTileList;
  begin
  Map:=PreviewMap(StartLandMass);
  MiniWidth:=lxpre[WorldSize]; MiniHeight:=lypre[WorldSize];

  Mini.PixelFormat:=pf24bit;
  Mini.Width:=MiniWidth*2;Mini.Height:=MiniHeight;
  for y:=0 to MiniHeight-1 do
    begin
    MiniLine:=Mini.ScanLine[y];
    for x:=0 to MiniWidth-1 do for i:=0 to 1 do
      begin
      xm:=(x*2+i+y and 1) mod (MiniWidth*2);
      cm:=MiniColors[Map[x*lxmax div MiniWidth
        +lxmax*((y*(lymax-1)+MiniHeight div 2) div (MiniHeight-1))] and fTerrain,i];
      MiniLine[xm,0]:=cm shr 16 *Brightness div 3;
      MiniLine[xm,1]:=cm shr 8 and $FF *Brightness div 3;
      MiniLine[xm,2]:=cm and $FF *Brightness div 3;
      end;
    end;
  end;

var
SaveMap: array[0..lxmax*lymax-1] of Byte;

  procedure PaintFileMini;
  type
  TLine=array[0..99999999,0..2] of Byte;
  var
  i,x,y,xm,cm,Tile,OwnColor,EnemyColor: integer;
  MiniLine,PrevMiniLine:^TLine;
  begin
  OwnColor:=GrExt[HGrSystem].Data.Canvas.Pixels[95,67];
  EnemyColor:=GrExt[HGrSystem].Data.Canvas.Pixels[96,67];
  Mini.PixelFormat:=pf24bit;
  Mini.Width:=MiniWidth*2;Mini.Height:=MiniHeight;
  if MiniMode=mmPicture then
    begin
    MiniLine:=nil;
    for y:=0 to MiniHeight-1 do
      begin
      PrevMiniLine:=MiniLine;
      MiniLine:=Mini.ScanLine[y];
      for x:=0 to MiniWidth-1 do for i:=0 to 1 do
        begin
        xm:=(x*2+i+y and 1) mod (MiniWidth*2);
        Tile:=SaveMap[x+MiniWidth*y];
        if Tile and fTerrain=fUNKNOWN then cm:=$000000
        else if Tile and smCity<>0 then
          begin
          if Tile and smOwned<>0 then cm:=OwnColor
          else cm:=EnemyColor;
          if PrevMiniLine<>nil then
            begin // 2x2 city dot covers two scanlines
            PrevMiniLine[xm,0]:=cm shr 16;
            PrevMiniLine[xm,1]:=cm shr 8 and $FF;
            PrevMiniLine[xm,2]:=cm and $FF;
            end
          end
        else if (i=0) and (Tile and smUnit<>0) then
          if Tile and smOwned<>0 then cm:=OwnColor
          else cm:=EnemyColor
        else cm:=MiniColors[Tile and fTerrain,i];
        MiniLine[xm,0]:=cm shr 16;
        MiniLine[xm,1]:=cm shr 8 and $FF;
        MiniLine[xm,2]:=cm and $FF;
        end;
      end
    end;
  end;

var
x,y,dummy, FileLandMass, lxFile, lyFile: integer;
LogFile, MapFile: file;
s: string[255];
MapRow: array[0..lxmax-1] of Cardinal;

begin
case Page of
  pgStartRandom:
    begin
    MiniMode:=mmPicture;
    PaintRandomMini(3);
    end;

  pgNoLoad:
    begin
    MiniWidth:=lxpre[DefaultWorldSize]; MiniHeight:=lypre[DefaultWorldSize];
    MiniMode:=mmNone;
    end;

  pgLoad:
    begin
    AssignFile(LogFile,DataDir+'Saved\'+List.Items[List.ItemIndex]+'.cevo');
    try
      Reset(LogFile,4);
      BlockRead(LogFile,s[1],2); {file id}
      BlockRead(LogFile,dummy,1); {format id}
      if dummy>=$000E01 then
        BlockRead(LogFile,dummy,1); {item stored since 0.14.1}
      BlockRead(LogFile,MiniWidth,1);
      BlockRead(LogFile,MiniHeight,1);
      BlockRead(LogFile,FileLandMass,1);
      if FileLandMass=0 then
        for y:=0 to MiniHeight-1 do BlockRead(LogFile,MapRow,MiniWidth);
      BlockRead(LogFile,dummy,1);
      BlockRead(LogFile,dummy,1);
      BlockRead(LogFile,LastTurn,1);
      BlockRead(LogFile,SaveMap,1);
      if SaveMap[0]=$80 then MiniMode:=mmMultiPlayer
      else MiniMode:=mmPicture;
      if MiniMode=mmPicture then BlockRead(LogFile,SaveMap[4],(MiniWidth*MiniHeight-1) div 4);
      CloseFile(LogFile);
    except
      CloseFile(LogFile);
      LastTurn:=0;
      MiniWidth:=lxpre[DefaultWorldSize]; MiniHeight:=lypre[DefaultWorldSize];
      MiniMode:=mmNone;
      end;
    //BookDate:=DateToStr(FileDateToDateTime(FileAge(FileName)));
    PaintFileMini;
    if not TurnValid then
      begin
      LoadTurn:=LastTurn;
      SmartInvalidate(xTurnSlider-2,y0Mini+61,xTurnSlider+wTurnSlider+2,yTurnSlider+9);
      end;
    TurnValid:=true;
    end;

  pgEditRandom:
    begin
    MapFileName:='';
    MiniMode:=mmPicture;
    PaintRandomMini(4);
    end;

  pgStartMap,pgEditMap:
    begin
    MiniMode:=mmPicture;
    if Page=pgEditMap then MapFileName:=List.Items[List.ItemIndex]+'.cevo map';
    if LoadGraphicFile(Mini, 'Maps\'+Copy(MapFileName,1,Length(MapFileName)-9), gfNoError) then
      begin
      if Mini.Width div 2>MaxWidthMapLogo then Mini.Width:=MaxWidthMapLogo*2;
      if Mini.Height>MaxHeightMapLogo then Mini.Height:=MaxHeightMapLogo;
      MiniWidth:=Mini.Width div 2;
      MiniHeight:=Mini.Height;
      end
    else
      begin
      MiniMode:=mmNone;
      MiniWidth:=MaxWidthMapLogo; MiniHeight:=MaxHeightMapLogo;
      end;

    AssignFile(MapFile,DataDir+'Maps\'+MapFileName);
    try
      Reset(MapFile,4);
      BlockRead(MapFile,s[1],2); {file id}
      BlockRead(MapFile,x,1); {format id}
      BlockRead(MapFile,x,1); //MaxTurn
      BlockRead(MapFile,lxFile,1);
      BlockRead(MapFile,lyFile,1);
      nMapStartPositions:=0;
      for y:=0 to lyFile-1 do
        begin
        BlockRead(MapFile,MapRow,lxFile);
        for x:=0 to lxFile-1 do
          if MapRow[x] and (fPrefStartPos or fStartPos)<>0 then
            inc(nMapStartPositions);
        end;
      if nMapStartPositions>nPl then nMapStartPositions:=nPl;
      CloseFile(MapFile);
    except
      CloseFile(MapFile);
      end;
    end
  end;
SmartInvalidate(x0Mini-lxmax,y0Mini-lymax div 2,
  x0Mini-lxmax+2*lxmax+4,y0Mini-lymax div 2+lymax+4);
end;

procedure TStartDlg.BrainClick(Sender: TObject);
var
i: integer;
begin
//Play('BUTTON_UP');
if bixPopup<0 then
  begin // change default AI
  bixDefault:=TMenuItem(Sender).Tag;
  SmartInvalidate(xDefault,yDefault,xDefault+64,yDefault+64);
  end
else
  begin
  Brain[bixView[bixPopup]].Flags:=Brain[bixView[bixPopup]].Flags and not fUsed;
  bixView[bixPopup]:=TMenuItem(Sender).Tag;
  DiffUpBtn[bixPopup].Visible:= bixView[bixPopup]>=bixTerm;
  DiffDownBtn[bixPopup].Visible:= bixView[bixPopup]>=bixTerm;
  if bixPopup in OfferMultiple then
    begin
    MultiBtn[bixPopup].Visible:= bixView[bixPopup]>=bixTerm;
    MultiBtn[bixPopup].ButtonIndex:=2+(MultiControl shr bixPopup) and 1;
    end;
  Brain[bixView[bixPopup]].Flags:=Brain[bixView[bixPopup]].Flags or fUsed;
  if bixView[bixPopup]<bixTerm then Difficulty[bixPopup]:=0 {supervisor}
  else Difficulty[bixPopup]:=2;
  if (Page=pgStartRandom) and (bixPopup in OfferMultiple)
    and (bixView[bixPopup]<0) then
    MultiControl:=MultiControl and not (1 shl bixPopup);
  if (bixPopup=0) and (MapFileName<>'') then ChangePage(Page);
  if bixView[bixPopup]=bixNoTerm then
    begin // turn all local players off
    for i:=1 to nPlOffered-1 do if bixView[i]=bixTerm then
      begin
      bixView[i]:=-1;
      DiffUpBtn[i].Visible:=false;
      DiffUpBtn[i].Tag:=0;
      DiffDownBtn[i].Visible:=false;
      DiffDownBtn[i].Tag:=0;
      if i in OfferMultiple then
        begin
        MultiBtn[i].Visible:=false;
        MultiBtn[i].Tag:=0;
        end;
      SmartInvalidate(xBrain[i]-31,yBrain[i]-1,xBrain[i]+64,DiffUpBtn[i].Top+25);
      end;
    Brain[bixTerm].Flags:=Brain[bixTerm].Flags and not fUsed;
    end;
  SmartInvalidate(xBrain[bixPopup]-31,yBrain[bixPopup]-1,xBrain[bixPopup]+64,
    DiffUpBtn[bixPopup].Top+25);
  end
end;

procedure TStartDlg.InitPopup(PopupIndex: integer);
var
i, FixedLines: integer;
m: TMenuItem;

  procedure OfferBrain(Index: integer);
  var
  j: integer;
  begin
  m:=TMenuItem.Create(PopupMenu1);
  if Index<0 then m.Caption:=Phrases.Lookup('NOMOD')
  else m.Caption:=Brain[Index].Name;
  m.Tag:=Index;
  m.OnClick:=BrainClick;
  j:=FixedLines;
  while (j<PopupMenu1.Items.Count) and (StrIComp(pchar(m.Caption),
    pchar(PopupMenu1.Items[j].Caption))>0) do inc(j);
  m.RadioItem:=true;
  if bixPopup<0 then m.Checked:= bixDefault=Index
  else m.Checked:= bixView[bixPopup]=Index;
  PopupMenu1.Items.Insert(j,m);
  end;

begin
bixPopup:=PopupIndex;
while PopupMenu1.Items.Count>0 do PopupMenu1.Items.Delete(0);
if bixPopup<0 then
  begin // select default AI
  FixedLines:=0;
  if nBrain>=bixFirstAI+2 then
    begin OfferBrain(bixRandom); inc(FixedLines) end;
  for i:=bixFirstAI to nBrain-1 do // offer available AIs
    if Brain[i].Flags and fMultiple<>0 then
      OfferBrain(i);
  end
else
  begin
  FixedLines:=0;
  if bixPopup>0 then begin OfferBrain(-1); inc(FixedLines); end;
  for i:=bixTerm downto 0 do // offer game interfaces
    if (bixPopup=0) or (i=bixTerm) and (bixView[0]<>bixNoTerm) then
      begin OfferBrain(i); inc(FixedLines); end;
  if bixPopup>0 then
    begin
    m:=TMenuItem.Create(PopupMenu1);
    m.Caption:='-';
    PopupMenu1.Items.Add(m);
    inc(FixedLines);
    if nBrain>=bixFirstAI+2 then
      begin OfferBrain(bixRandom); inc(FixedLines); end;
    for i:=bixFirstAI to nBrain-1 do // offer available AIs
      if (Brain[i].Flags and fMultiple<>0) or (Brain[i].Flags and fUsed=0)
        or (i=bixView[bixPopup]) then
        OfferBrain(i);
    end;
  end
end;

procedure TStartDlg.UpdateFormerGames;
var
i: integer;
f: TSearchRec;
begin
FormerGames.Clear;
if FindFirst(DataDir+'Saved\*.cevo',$21,f)=0 then
  repeat
    i:=FormerGames.Count;
    while (i>0) and (f.Time<integer(FormerGames.Objects[i-1])) do
      dec(i);
    FormerGames.InsertObject(i,Copy(f.Name,1,Length(f.Name)-5),
      TObject(f.Time));
  until FindNext(f)<>0;
ListIndex[2]:=FormerGames.Count-1;
if (ShowTab=1) and (FormerGames.Count>0) then ShowTab:=2;
TurnValid:=false;
end;

procedure TStartDlg.UpdateMaps;
var
f: TSearchRec;
begin
Maps.Clear;
if FindFirst(DataDir+'Maps\*.cevo map',$21,f)=0 then
  repeat
    Maps.Add(Copy(f.Name,1,Length(f.Name)-9));
  until FindNext(f)<>0;
Maps.Sort;
Maps.Insert(0,Phrases.Lookup('RANMAP'));
ListIndex[0]:=Maps.IndexOf(Copy(MapFileName,1,Length(MapFileName)-9));
if ListIndex[0]<0 then ListIndex[0]:=0;
end;

procedure TStartDlg.ChangePage(NewPage: integer);
var
i,j,p1: integer;
s: string;
Reg: TRegistry;
begin
Page:=NewPage;
case Page of
  pgStartRandom, pgStartMap:
    begin
    StartBtn.Caption:=Phrases.Lookup('STARTCONTROLS',1);
    if Page=pgStartRandom then i:=nPlOffered
    else
      begin
      i:=nMapStartPositions;
      if i=0 then begin bixView[0]:=bixSuper_Virtual; Difficulty[0]:=0 end;
      if bixView[0]<bixTerm then inc(i);
      if i>nPl then i:=nPl;
      if i<=nPlOffered then MultiControl:=0
      else MultiControl:=InitMulti[i];
      end;
    if InitAlive[i]<>SlotAvailable then
      if Page=pgStartRandom then
        begin // restore AI assignment of last start
        Reg:=TRegistry.Create;
        Reg.OpenKey('SOFTWARE\cevo\RegVer8\Start',false);
        for p1:=0 to nPlOffered-1 do
          begin
          bixView[p1]:=-1;
          s:=Reg.ReadString('Control'+IntToStr(p1));
          Difficulty[p1]:=Reg.ReadInteger('Diff'+IntToStr(p1));
          if s<>'' then
            for j:=0 to nBrain-1 do
              if AnsiCompareFileName(s,Brain[j].FileName)=0 then bixView[p1]:=j;
          end;
        MultiControl:=Reg.ReadInteger('MultiControl');
        Reg.closekey;
        Reg.Free;
        end
      else
        for p1:=1 to nPl-1 do
          if 1 shl p1 and InitAlive[i]<>0 then
            begin bixView[p1]:=bixDefault; Difficulty[p1]:=2; end
          else bixView[p1]:=-1;
    SlotAvailable:=InitAlive[i];
    for i:=0 to nPlOffered-1 do
      if (AutoDiff<0) and (bixView[i]>=bixTerm) then
        begin DiffUpBtn[i].Tag:=768; DiffDownBtn[i].Tag:=768; end
      else begin DiffUpBtn[i].Tag:=0; DiffDownBtn[i].Tag:=0; end;
    for i:=6 to 8 do
      if (AutoDiff<0) and (bixView[i]>=bixTerm) then
        begin
        MultiBtn[i].Tag:=768;
        MultiBtn[i].ButtonIndex:=2+(MultiControl shr i) and 1;
        MultiBtn[i].Enabled:=Page=pgStartRandom
        end
      else MultiBtn[i].Tag:=0;
    if (AutoDiff>0) and (Page<>pgStartMap) then
      begin
      AutoEnemyUpBtn.Tag:=768;
      AutoEnemyDownBtn.Tag:=768;
      end
    else
      begin
      AutoEnemyUpBtn.Tag:=0;
      AutoEnemyDownBtn.Tag:=0;
      end;
    if AutoDiff>0 then
      begin
      AutoDiffUpBtn.Tag:=768;
      AutoDiffDownBtn.Tag:=768;
      end
    else
      begin
      AutoDiffUpBtn.Tag:=0;
      AutoDiffDownBtn.Tag:=0;
      end
    end;

  pgNoLoad,pgLoad:
    begin
    StartBtn.Caption:=Phrases.Lookup('STARTCONTROLS',2);
    RenameBtn.Hint:=Phrases.Lookup('BTN_RENGAME');
    DeleteBtn.Hint:=Phrases.Lookup('BTN_DELGAME');
    end;

  pgEditRandom,pgEditMap:
    begin
    StartBtn.Caption:=Phrases.Lookup('STARTCONTROLS',12);
    RenameBtn.Hint:=Phrases.Lookup('BTN_RENMAP');
    DeleteBtn.Hint:=Phrases.Lookup('BTN_DELMAP');
    end;
  end;

PaintInfo;
for i:=0 to ControlCount-1 do
  Controls[i].Visible:= Controls[i].Tag and (256 shl Page)<>0;
if Page=pgLoad then
  ReplayBtn.Visible:= MiniMode<>mmMultiPlayer;
List.Invalidate;
SmartInvalidate(0,0,ClientWidth,ClientHeight);
end;

procedure TStartDlg.ChangeTab(NewTab: integer);
begin
Tab:=NewTab;
case Tab of
  0: List.Items.Assign(Maps);
  2: List.Items.Assign(FormerGames);
  end;
if Tab<>1 then
  if ListIndex[Tab]>=0 then List.ItemIndex:=ListIndex[Tab]
  else List.ItemIndex:=0;
case Tab of
  0:
    if List.ItemIndex=0 then ChangePage(pgEditRandom)
    else ChangePage(pgEditMap);
  1:
    if MapFileName='' then ChangePage(pgStartRandom)
    else ChangePage(pgStartMap);
  2:
    if FormerGames.Count=0 then ChangePage(pgNoLoad)
    else ChangePage(pgLoad);
  end;
end;

procedure TStartDlg.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: integer);
var
i: integer;
begin
if (y<TabHeight+1) and (x<TabSize*3) and (x div TabSize<>Tab) then
  begin
//  Play('BUTTON_DOWN');
  ListIndex[Tab]:=List.ItemIndex;
  ChangeTab(x div TabSize);
  end
else if (AutoDiff<0) and ((page=pgStartRandom)
  or (page=pgStartMap) and (nMapStartPositions>0)) then
  begin
  for i:=0 to nPlOffered-1 do
    if (1 shl i and SlotAvailable<>0) and (x>=xBrain[i]) and (y>=yBrain[i])
      and (x<xBrain[i]+64) and (y<yBrain[i]+64) then
      begin
      InitPopup(i);
      if yBrain[i]>y0Brain then
        PopupMenu1.Popup(Left+xBrain[i]+4,Top+yBrain[i]+60)
      else PopupMenu1.Popup(Left+xBrain[i]+4,Top+yBrain[i]+4);
      end
  end
else if (AutoDiff>0) and ((page=pgStartRandom) or (page=pgStartMap))
  and (x>=xDefault) and (y>=yDefault) and (x<xDefault+64) and (y<yDefault+64) then
    if nBrain<bixFirstAI+2 then
      SimpleMessage(Phrases.Lookup('NOALTAI'))
    else
      begin
      InitPopup(-1);
      PopupMenu1.Popup(Left+xDefault+4,Top+yDefault+4);
      end
else if (page=pgLoad) and (LastTurn>0) and (y>=yTurnSlider) and (y<yTurnSlider+7)
  and (x>=xTurnSlider) and (x<=xTurnSlider+wTurnSlider) then
    begin
    LoadTurn:=LastTurn*(x-xTurnSlider) div wTurnSlider;
    SmartInvalidate(xTurnSlider-2,y0Mini+61,xTurnSlider+wTurnSlider+2,yTurnSlider+9);
    Tracking:=true
    end
end;

procedure TStartDlg.Up2BtnClick(Sender: TObject);
begin
case Page of
  pgStartRandom,pgStartMap:
    if MaxTurn<1400 then
      begin
      inc(MaxTurn,200);
      SmartInvalidate(344,y0Mini+61,514,y0Mini+82);
      end;
  pgLoad:
    if LoadTurn<LastTurn then
      begin
      inc(LoadTurn);
      SmartInvalidate(xTurnSlider-2,y0Mini+61,xTurnSlider+wTurnSlider+2,yTurnSlider+9);
      end;
  pgEditRandom:
    if StartLandMass<96 then
      begin
      inc(StartLandMass,5);
      PaintInfo;
      SmartInvalidate(344,y0Mini+61,514,y0Mini+61+21);
      end;
  end
end;

procedure TStartDlg.Down2BtnClick(Sender: TObject);
begin
case Page of
  pgStartRandom,pgStartMap:
    if MaxTurn>400 then
      begin
      dec(MaxTurn,200);
      SmartInvalidate(344,y0Mini+61,514,y0Mini+82);
      end;
  pgLoad:
    if LoadTurn>0 then
      begin
      dec(LoadTurn);
      SmartInvalidate(xTurnSlider-2,y0Mini+61,xTurnSlider+wTurnSlider+2,yTurnSlider+9);
      end;
  pgEditRandom:
    if StartLandMass>10 then
      begin
      dec(StartLandMass,5);
      PaintInfo;
      SmartInvalidate(344,y0Mini+61,514,y0Mini+61+21);
      end
  end
end;

procedure TStartDlg.Up1BtnClick(Sender: TObject);
begin
if WorldSize<nWorldSize-1 then
  begin
  inc(WorldSize);
  PaintInfo;
  SmartInvalidate(344,y0Mini-77,510,y0Mini-77+21);
  end
end;

procedure TStartDlg.Down1BtnClick(Sender: TObject);
begin
if WorldSize>0 then
  begin
  dec(WorldSize);
  PaintInfo;
  SmartInvalidate(344,y0Mini-77,510,y0Mini-77+21);
  end
end;

procedure TStartDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
DirectDlg.Close
end;

procedure TStartDlg.ListClick(Sender: TObject);
var
i: integer;
begin
if (Tab=0) and ((List.ItemIndex=0)<>(Page=pgEditRandom)) then
  begin
  if List.ItemIndex=0 then Page:=pgEditRandom
  else Page:=pgEditMap;
  for i:=0 to ControlCount-1 do
    Controls[i].Visible:= Controls[i].Tag and (256 shl Page)<>0;
  SmartInvalidate(328,Up1Btn.Top-12,ClientWidth,Up2Btn.Top+35);
  end;
if Page=pgLoad then TurnValid:=false;
PaintInfo;
if Page=pgLoad then
  ReplayBtn.Visible:= MiniMode<>mmMultiPlayer;
end;

procedure TStartDlg.RenameBtnClick(Sender: TObject);
var
i: integer;
NewName: string;
f: file;
ok: boolean;
begin
if List.ItemIndex>=0 then
  begin
  if Page=pgLoad then InputDlg.Caption:=Phrases.Lookup('TITLE_BOOKNAME')
  else InputDlg.Caption:=Phrases.Lookup('TITLE_MAPNAME');
  InputDlg.EInput.Text:=List.Items[List.ItemIndex];
  InputDlg.CenterToRect(BoundsRect);
  InputDlg.ShowModal;
  NewName:=InputDlg.EInput.Text;
  while (NewName<>'') and (NewName[1]='~') do delete(NewName,1,1);
  if (InputDlg.ModalResult=mrOK) and (NewName<>'')
    and (NewName<>List.Items[List.ItemIndex]) then
    begin
    for i:=1 to Length(NewName) do
      if NewName[i] in ['\','/',':','*','?','"','<','>','|'] then
        begin
        SimpleMessage(Format(Phrases.Lookup('NOFILENAME'),[NewName[i]]));
        exit
        end;
    if Page=pgLoad then
      AssignFile(f,DataDir+'Saved\'+List.Items[List.ItemIndex]+'.cevo')
    else AssignFile(f,DataDir+'Maps\'+List.Items[List.ItemIndex]+'.cevo map');
    ok:=true;
    try
      if Page=pgLoad then
        Rename(f,DataDir+'Saved\'+NewName+'.cevo')
      else Rename(f,DataDir+'Maps\'+NewName+'.cevo map');
    except
//      Play('INVALID');
      ok:=false
      end;
    if Page<>pgLoad then
      try // rename map picture
        AssignFile(f,DataDir+'Maps\'+List.Items[List.ItemIndex]+'.bmp');
        Rename(f,DataDir+'Maps\'+NewName+'.bmp');
      except
        end;
    if ok then
      begin
      if Page=pgLoad then
        FormerGames[List.ItemIndex]:=NewName
      else Maps[List.ItemIndex]:=NewName;
      List.Items[List.ItemIndex]:=NewName;
      if Page=pgEditMap then PaintInfo;
      List.Invalidate;
      end
    end
  end
end;

procedure TStartDlg.DeleteBtnClick(Sender: TObject);
var
iDel: integer;
f: file;
begin
if List.ItemIndex>=0 then
  begin
  if Page=pgLoad then MessgDlg.MessgText:=Phrases.Lookup('DELETEQUERY')
  else MessgDlg.MessgText:=Phrases.Lookup('MAPDELETEQUERY');
  MessgDlg.Kind:=mkOKCancel;
  MessgDlg.ShowModal;
  if MessgDlg.ModalResult=mrOK then
    begin
    if Page=pgLoad then
      AssignFile(f,DataDir+'Saved\'+List.Items[List.ItemIndex]+'.cevo')
    else AssignFile(f,DataDir+'Maps\'+List.Items[List.ItemIndex]+'.cevo map');
    Erase(f);
    iDel:=List.ItemIndex;
    if Page=pgLoad then FormerGames.Delete(iDel)
    else Maps.Delete(iDel);
    List.Items.Delete(iDel);
    if List.Items.Count=0 then ChangePage(pgNoLoad)
    else
      begin
      if iDel=0 then List.ItemIndex:=0
      else List.ItemIndex:=iDel-1;
      if (Page=pgEditMap) and (List.ItemIndex=0) then ChangePage(pgEditRandom)
      else
        begin List.Invalidate;
        if Page=pgLoad then TurnValid:=false;
        PaintInfo;
        if Page=pgLoad then
          ReplayBtn.Visible:= MiniMode<>mmMultiPlayer;
        end;
      end
    end
  end
end;

procedure TStartDlg.DiffBtnClick(Sender: TObject);
var
i: integer;
begin
for i:=0 to nPlOffered-1 do
  if (Sender=DiffUpBtn[i]) and (Difficulty[i]<MaxDiff)
    or (Sender=DiffDownBtn[i]) and (Difficulty[i]>1) then
    begin
    if Sender=DiffUpBtn[i] then inc(Difficulty[i])
    else dec(Difficulty[i]);
    SmartInvalidate(xBrain[i]-18,yBrain[i]+19,xBrain[i]-18+12,yBrain[i]+(19+14));
    end
end;

procedure TStartDlg.MultiBtnClick(Sender: TObject);
var
i: integer;
begin
for i:=6 to 8 do if Sender=MultiBtn[i] then
  begin
  MultiControl:=MultiControl xor (1 shl i);
  TButtonC(Sender).ButtonIndex:=2+(MultiControl shr i) and 1;
  end
end;

procedure TStartDlg.FormHide(Sender: TObject);
begin
Diff0:=Difficulty[0];
ListIndex[Tab]:=List.ItemIndex;
ShowTab:=Tab;
end;

procedure TStartDlg.QuitBtnClick(Sender: TObject);
begin
Close
end;

procedure TStartDlg.HelpBtnClick(Sender: TObject);
begin
DirectHelp(true);
end;

procedure TStartDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
if (Shift=[]) and (Key=VK_F1) then DirectHelp(true);
end;

procedure TStartDlg.CustomizeBtnClick(Sender: TObject);
begin
AutoDiff:=-AutoDiff;
CustomizeBtn.ButtonIndex:=CustomizeBtn.ButtonIndex xor 1;
ChangePage(Page)
end;

procedure TStartDlg.AutoDiffUpBtnClick(Sender: TObject);
begin
if AutoDiff<5 then
  begin
  inc(AutoDiff);
  SmartInvalidate(120,y0Mini+61,272,y0Mini+61+21);
  end
end;

procedure TStartDlg.AutoDiffDownBtnClick(Sender: TObject);
begin
if AutoDiff>1 then
  begin
  dec(AutoDiff);
  SmartInvalidate(120,y0Mini+61,272,y0Mini+61+21);
  end
end;

procedure TStartDlg.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
Tracking:=false
end;

procedure TStartDlg.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
OldLoadTurn: integer;
begin
if Tracking then
  begin
  x:=x-xTurnSlider;
  if x<0 then x:=0
  else if x>wTurnSlider then x:=wTurnSlider;
  OldLoadTurn:=LoadTurn;
  LoadTurn:=LastTurn*x div wTurnSlider;
  if LoadTurn<OldLoadTurn then
    begin
    SmartInvalidate(xTurnSlider+LoadTurn*wTurnSlider div LastTurn,yTurnSlider,
      xTurnSlider+OldLoadTurn*wTurnSlider div LastTurn+1,yTurnSlider+7);
    SmartInvalidate(344,y0Mini+61,514,y0Mini+82);
    end
  else if LoadTurn>OldLoadTurn then
    begin
    SmartInvalidate(xTurnSlider+OldLoadTurn*wTurnSlider div LastTurn,yTurnSlider,
      xTurnSlider+LoadTurn*wTurnSlider div LastTurn+1,yTurnSlider+7);
    SmartInvalidate(344,y0Mini+61,514,y0Mini+82);
    end;
  end
end;

procedure TStartDlg.AutoEnemyUpBtnClick(Sender: TObject);
begin
if AutoEnemies<nPl-1 then
  begin
  inc(AutoEnemies);
  SmartInvalidate(160,140,198,140+21);
  end
end;

procedure TStartDlg.AutoEnemyDownBtnClick(Sender: TObject);
begin
if AutoEnemies>0 then
  begin
  dec(AutoEnemies);
  SmartInvalidate(160,140,198,140+21);
  end
end;

procedure TStartDlg.ReplayBtnClick(Sender: TObject);
begin
LoadGame(DataDir+'Saved\', List.Items[List.ItemIndex]+'.cevo', LastTurn, true);
SlotAvailable:=-1;
end;

end.

