{$INCLUDE switches}

unit Messg;

interface

uses
  ScreenTools,

  Windows,Messages,SysUtils,Classes,Graphics,Controls,Forms,ButtonA,
  ButtonBase;

const
WM_PLAYSOUND=WM_USER;

type
  TBaseMessgDlg = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender:TObject);
  public
    MessgText: string;
  protected
    Lines, TopSpace: integer;
    procedure SplitText(preview: boolean);
    procedure CorrectHeight;
    procedure OnEraseBkgnd(var m:TMessage); message WM_ERASEBKGND;
    procedure OnHitTest(var Msg:TMessage); message WM_NCHITTEST;
  end;

  TMessgDlg = class(TBaseMessgDlg)
    Button1: TButtonA;
    Button2: TButtonA;
    procedure FormCreate(Sender:TObject);
    procedure FormPaint(Sender:TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
  public
    Kind: integer;
    OpenSound: string;
  private
    LinkBounds: TRect;
    procedure OnPlaySound(var Msg:TMessage); message WM_PLAYSOUND;
  end;

const
// message kinds
mkOK=1; mkOKCancel=2; mkYesNo=3;

Border=3;
MessageLineSpacing=20;

var
  MessgDlg:TMessgDlg;

procedure SimpleMessage(SimpleText: string);
procedure SoundMessage(SimpleText, SoundItem: string);


implementation

{$R *.DFM}

procedure TBaseMessgDlg.FormCreate(Sender: TObject);
begin
Left:=(Screen.Width-ClientWidth) div 2;
Canvas.Font.Assign(UniFont[ftNormal]);
Canvas.Brush.Style:=bsClear;
MessgText:='';
TopSpace:=0;
InitButtons(self);
end;

procedure TBaseMessgDlg.FormPaint(Sender:TObject);
var
i,cix: integer;
begin
PaintBackground(self,3+Border,3+Border,ClientWidth-(6+2*Border),
  ClientHeight-(6+2*Border));
for i:=0 to Border do
  Frame(Canvas,i,i,ClientWidth-1-i,ClientHeight-1-i,
    $000000,$000000);
Frame(Canvas,Border+1,Border+1,ClientWidth-(2+Border),ClientHeight-(2+Border),
  MainTexture.clBevelLight,MainTexture.clBevelShade);
Frame(Canvas,2+Border,2+Border,ClientWidth-(3+Border),ClientHeight-(3+Border),
  MainTexture.clBevelLight,MainTexture.clBevelShade);
SplitText(false);

for cix:=0 to ControlCount-1 do
  if (Controls[cix].Visible) and (Controls[cix] is TButtonBase) then
    BtnFrame(Canvas,Controls[cix].BoundsRect,MainTexture);
end;

procedure TBaseMessgDlg.SplitText(preview: boolean);
var
Start,Stop,OrdinaryStop,LinesCount: integer;
s: string;
begin
Start:=1;
LinesCount:=0;
while Start<Length(MessgText) do
  begin
  Stop:=Start;
  while(Stop<Length(MessgText)) and (MessgText[Stop]<>'\')
    and (BiColorTextWidth(Canvas,Copy(MessgText,Start,Stop-Start+1))<ClientWidth-56) do inc(Stop);
  if Stop<>Length(MessgText) then
    begin
    OrdinaryStop:=Stop;
    repeat dec(OrdinaryStop)
    until (MessgText[OrdinaryStop+1]=' ') or (MessgText[OrdinaryStop+1]='\');
    if (OrdinaryStop+1-Start)*2>=Stop-Start then
      Stop:=OrdinaryStop
    end;
  if not preview then
    begin
    s:=Copy(MessgText,Start,Stop-Start+1);
    LoweredTextOut(Canvas,-1,MainTexture,
      (ClientWidth-BiColorTextWidth(Canvas,s)) div 2,19+Border+TopSpace+LinesCount*MessageLineSpacing,s);
    end;
  Start:=Stop+2;
  inc(LinesCount)
  end;
if preview then Lines:=LinesCount;
end;

procedure TBaseMessgDlg.CorrectHeight;
var
i: integer;
begin
ClientHeight:=72+Border+TopSpace+Lines*MessageLineSpacing;
Top:=(Screen.Height-ClientHeight) div 2;
for i:=0 to ControlCount-1 do
  Controls[i].Top:=ClientHeight-(34+Border);
end;

procedure TBaseMessgDlg.OnEraseBkgnd(var m:TMessage);
begin
end;

procedure TBaseMessgDlg.OnHitTest(var Msg:TMessage);
begin
if (Msg.LParamHi>=Top+ClientHeight-(34+Border)) then
  Msg.result:=HTCLIENT
else Msg.result:=HTCAPTION
end;

procedure TMessgDlg.FormCreate(Sender:TObject);
begin
inherited;
OpenSound:='';
end;

procedure TMessgDlg.FormShow(Sender: TObject);
begin
Button1.Visible:=true;
Button2.Visible:= not (Kind in [mkOK]);
if Button2.Visible then Button1.Left:=97
else Button1.Left:=155;
if Kind=mkYesNo then
  begin
  Button1.Caption:=Phrases.Lookup('BTN_YES');
  Button2.Caption:=Phrases.Lookup('BTN_NO')
  end
else
  begin
  Button1.Caption:=Phrases.Lookup('BTN_OK');
  Button2.Caption:=Phrases.Lookup('BTN_CANCEL');
  end;

SplitText(true);
CorrectHeight;
end;

procedure TMessgDlg.FormPaint(Sender:TObject);
begin
inherited;
if OpenSound<>'' then PostMessage(Handle, WM_PLAYSOUND, 0, 0);
end; {FormPaint}

procedure TMessgDlg.Button1Click(Sender: TObject);
begin
ModalResult:=mrOK;
end;

procedure TMessgDlg.Button2Click(Sender: TObject);
begin
ModalResult:=mrIgnore;
end;

procedure TMessgDlg.FormKeyPress(Sender: TObject; var Key: char);
begin
if Key=#13 then ModalResult:=mrOK
//else if (Key=#27) and (Button2.Visible) then ModalResult:=mrCancel
end;

procedure SimpleMessage(SimpleText: string);
begin
with MessgDlg do
  begin
  MessgText:=SimpleText;
  Kind:=mkOK;
  ShowModal;
  end
end;

procedure SoundMessage(SimpleText, SoundItem: string);
begin
with MessgDlg do
  begin
  MessgText:=SimpleText;
  OpenSound:=SoundItem;
  Kind:=mkOK;
  ShowModal;
  end
end;

procedure TMessgDlg.OnPlaySound(var Msg:TMessage);
begin
Play(OpenSound);
OpenSound:='';
end;

end.

