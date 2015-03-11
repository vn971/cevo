{$INCLUDE switches}

unit Draft;

interface

uses
  Protocol,ClientTools,Term,ScreenTools,PVSB,BaseWin,

  Windows,Messages,SysUtils,Classes,Graphics,Controls,Forms,ExtCtrls,ButtonA,
  ButtonB, ButtonBase;

type
  TDraftDlg = class(TFramedDlg)
    OKBtn: TButtonA;
    CloseBtn: TButtonA;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: integer);
    procedure OKBtnClick(Sender: TObject);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: integer);
  public
    procedure ShowNewContent(NewMode: integer); 
  protected
    procedure OffscreenPaint; override;
  private
    sb:TPVScrollbar;
    Domain,Lines,nCodes,nAllCodes,IncCap,DecCap: integer;
    code,AllCodes: array[0..nFeature-1] of integer;
    procedure SetDomain(d: integer);
    procedure OnScroll(var m:TMessage); message WM_VSCROLL;
    procedure OnMouseWheel(var m:TMessage); message WM_MOUSEWHEEL;
  end;

var
  DraftDlg: TDraftDlg;

implementation

uses Help,Tribes;

{$R *.DFM}

const
VisibleLines=8; LinePitch=20;
xDomain=9; yDomain=43; DomainPitch=26;
xFeature=84; yFeature=24;
xWeight=118; yWeight=yFeature+2+VisibleLines*LinePitch;
xTotal=99; yTotal=yFeature+29+VisibleLines*LinePitch;
xView=13; yView=yFeature+34+VisibleLines*LinePitch;
WeightHeight=24;

procedure TDraftDlg.FormCreate(Sender: TObject);
begin
inherited;
WideBottom:=true;
CreatePVSB(sb,Handle,36+yFeature-2,ClientWidth-SideFrame,
  36+yFeature-2+LinePitch*VisibleLines);
InnerHeight:=ClientHeight-2*WideFrame;
InitButtons(self);
HelpContext:='CLASSES';
Caption:=Phrases.Lookup('TITLE_DRAFT');
OKBtn.Caption:=Phrases.Lookup('BTN_OK');
CloseBtn.Caption:=Phrases.Lookup('BTN_CANCEL');
end;

procedure TDraftDlg.CloseBtnClick(Sender: TObject);
begin
ModalResult:=mrCancel;
end;

procedure TDraftDlg.OnScroll(var m:TMessage);
begin
if ProcessPVSB(sb,m) then
  SmartUpdateContent(true)
end;

procedure TDraftDlg.OnMouseWheel(var m:TMessage);
begin
if ProcessMouseWheel(sb,m) then
  SmartUpdateContent(true)
end;

procedure TDraftDlg.OffscreenPaint;

  procedure PaintTotalBars;
  var
  i,x,y,dx,num,w: integer;
  s: string;
  begin
  with offscreen.Canvas do
    begin
    // strength bar
    x:=xTotal; y:=yTotal;
    DarkGradient(Offscreen.Canvas,x-6,y+1,243+30,2);
    RisedTextOut(Offscreen.Canvas,x-2,y,Phrases.Lookup('UNITSTRENGTH'));
    RisedTextOut(Offscreen.Canvas,x+112+30,y,'x'+IntToStr(MyRO.DevModel.MStrength));
    RisedTextOut(Offscreen.Canvas,x+148+30,y,'=');
    s:=IntToStr(MyRO.DevModel.Attack)+'/'+IntToStr(MyRO.DevModel.Defense);
    RisedTextOut(Offscreen.Canvas,x+170+64+30-BiColorTextWidth(Offscreen.Canvas,s),y,s);

    // transport bar
    if MyRO.DevModel.MTrans>0 then
      begin
      x:=xTotal; y:=yTotal+19;
      DarkGradient(Offscreen.Canvas,x-6,y+1,243+30,1);
      RisedTextOut(Offscreen.Canvas,x-2,y,Phrases.Lookup('UNITTRANSPORT'));
      RisedTextOut(Offscreen.Canvas,x+112+30,y,'x'+IntToStr(MyRO.DevModel.MTrans));
      RisedTextOut(Offscreen.Canvas,x+148+30,y,'=');

      Font.Color:=$000000;
      dx:=-237-30;
      for i:=mcFirstNonCap-1 downto 3 do
        if i in [mcSeaTrans,mcCarrier,mcAirTrans] then
          begin
          num:=MyRO.DevModel.Cap[i]*MyRO.DevModel.MTrans;
          if num>0 then
            begin
            inc(dx,15);
            Brush.Color:=$C0C0C0;
            FrameRect(Rect(x-3-dx,y+2,x+11-dx,y+16));
            Brush.Style:=bsClear;
            Sprite(Offscreen,HGrSystem,x-1-dx,y+4,10,10,66+i mod 11 *11,137+i div 11 *11);
            if num>1 then
              begin
              s:=IntToStr(num);
              w:=TextWidth(s);
              inc(dx,w+1);
              Brush.Color:=$FFFFFF;
              FillRect(Rect(x-3-dx,y+2,x+w-1-dx,y+16));
              Brush.Style:=bsClear;
              Textout(x-3-dx+1,y,s);
              end;
            end;
          end
      end;

    // speed bar
    x:=xTotal; y:=yTotal+38;
    LoweredTextOut(offscreen.Canvas,-1,MainTexture,x-2,y,Phrases.Lookup('UNITSPEED'));
    DLine(offscreen.Canvas,x-2,x+263,y+16,MainTexture.clBevelShade,
      MainTexture.clBevelLight);
    s:=MovementToString(MyRO.DevModel.Speed);
    RisedTextOut(offscreen.Canvas,x+170+64+30-TextWidth(s),y,s);

    // cost bar
    x:=xTotal; y:=yTotal+57;
    LoweredTextOut(offscreen.Canvas,-1,MainTexture,x-2,y,Phrases.Lookup('UNITCOST'));
    LoweredTextOut(Offscreen.Canvas,-1,MainTexture,x+112+30,y,'x'+IntToStr(MyRO.DevModel.MCost));
    LoweredTextOut(Offscreen.Canvas,-1,MainTexture,x+148+30,y,'=');
    DLine(offscreen.Canvas,x-2,x+263,y+16,MainTexture.clBevelShade,
      MainTexture.clBevelLight);
    s:=IntToStr(MyRO.DevModel.Cost);
    RisedTextOut(offscreen.Canvas,x+170+64+30-12-TextWidth(s),y,s);
    Sprite(offscreen,HGrSystem,x+170+54+30,y+4,10,10,88,115);
    end;
  end;

var
i,j,x,y,d,n,TextColor,CapWeight: integer;
s: string;
begin
inherited;
FillOffscreen(1,1,InnerWidth-1,yFeature-4);
FillOffscreen(xFeature-30,yFeature-2,InnerWidth-xFeature+30,
  VisibleLines*LinePitch);
FillOffscreen(1,yFeature-1,xFeature-32,VisibleLines*LinePitch);
FillOffscreen(1,yFeature+VisibleLines*LinePitch-1,InnerWidth-2,WeightHeight-2);
FillOffscreen(0,yFeature+VisibleLines*LinePitch-2+WeightHeight,
  InnerWidth,InnerHeight-(yFeature+VisibleLines*LinePitch-2+WeightHeight));
Frame(offscreen.Canvas,0,0,InnerWidth-1,yFeature-3,
  MainTexture.clBevelLight,MainTexture.clBevelShade);
Frame(offscreen.Canvas,0,yFeature-2,xFeature-31,yFeature-3+VisibleLines*LinePitch,
  MainTexture.clBevelLight,MainTexture.clBevelShade);
Frame(offscreen.Canvas,0,yFeature-2+VisibleLines*LinePitch,InnerWidth-1,
  yFeature-2+VisibleLines*LinePitch+WeightHeight-1,MainTexture.clBevelLight,
  MainTexture.clBevelShade);
with offscreen.Canvas do
  begin
  Font.Assign(UniFont[ftSmall]);
  s:=Format(Phrases.Lookup('MODELDRAFT'),[Phrases.Lookup('DOMAIN',Domain)]);
  LoweredTextOut(offscreen.canvas,-1,MainTexture,
    (InnerWidth-BiColorTextWidth(offscreen.Canvas,s)) div 2,2,s);
  end;

with MyRO.DevModel do
  begin
  for d:=0 to nDomains-1 do
    if (upgrade[d,0].Preq=preNone)
      or (MyRO.Tech[upgrade[d,0].Preq]>=tsApplicable) then
      begin
      x:=xDomain; y:=yDomain+d*DomainPitch;
      Dump(offscreen,HGrSystem,x,y,36,20,75+d*37,295);
      if d=Domain then
        Frame(offscreen.Canvas,x-1,y-1,x+36,y+20,
          MainTexture.clBevelShade,MainTexture.clBevelLight)
      else Frame(offscreen.Canvas,x-1,y-1,x+36,y+20,
        MainTexture.clBevelLight,MainTexture.clBevelShade);
      Frame(offscreen.Canvas,x-2,y-2,x+37,y+21,
        MainTexture.clBevelShade,MainTexture.clBevelLight);
      end;

  PaintTotalBars;

  if G.Difficulty[me]<>2 then
    begin // corrected cost bar
    x:=xTotal; y:=yTotal+76;
    LoweredTextOut(offscreen.Canvas,-1,MainTexture,x-2,y,
      Phrases.Lookup('COSTDIFF'+char(48+G.Difficulty[me])));
    LoweredTextOut(Offscreen.Canvas,-1,MainTexture,x+148+30,y,'=');
    DLine(offscreen.Canvas,x-2,x+263,y+16,MainTexture.clBevelShade,
      MainTexture.clBevelLight);
    s:=IntToStr(MyRO.DevModel.Cost*BuildCostMod[G.Difficulty[me]] div 12);
    RisedTextOut(offscreen.Canvas,x+170+64+30-12-offscreen.Canvas.TextWidth(s),y,s);
    Sprite(offscreen,HGrSystem,x+170+54+30,y+4,10,10,88,115);
    end;

  // display weight
  with offscreen.Canvas do
    begin
    LightGradient(Offscreen.Canvas,xWeight,yWeight,138,
      GrExt[HGrSystem].Data.Canvas.Pixels[187,137]);
    for i:=0 to MaxWeight-1 do
      if i<Weight then
        Sprite(offscreen,HGrSystem,xWeight+4+6+10*6+12*i-MaxWeight*6,yWeight+3,10,10,88,126)
      else Sprite(offscreen,HGrSystem,xWeight+4+6+10*6+12*i-MaxWeight*6,yWeight+3,10,10,99,126)
    end;

  Lines:=0;
  nCodes:=0;
  for i:=0 to nAllCodes-1 do
    begin
    if (Lines>=sb.si.npos) and (Lines<sb.si.npos+VisibleLines) then
      begin code[nCodes]:=AllCodes[i]; inc(nCodes); end;
    inc(Lines)
    end;

  with offscreen.Canvas do for i:=0 to nCodes-1 do
    begin
    if not (code[i] in AutoFeature) then
      begin
      // paint +/- butttons
      if code[i]<mcFirstNonCap then
        begin
        Dump(offscreen,HGrSystem,xFeature-21,yFeature+2+LinePitch*i,
          12,12,169,172);
        Dump(offscreen,HGrSystem,xFeature-9,yFeature+2+LinePitch*i,
          12,12,169,159);
        RFrame(offscreen.Canvas,xFeature-(21+1),yFeature+2+LinePitch*i-1,
          xFeature-(21-24),yFeature+2+LinePitch*i+12,
          MainTexture.clBevelShade,MainTexture.clBevelLight);
        end
      else
        begin
        Dump(offscreen,HGrSystem,xFeature-9,yFeature+2+LinePitch*i,
          12,12,169,185+13*MyRO.DevModel.Cap[code[i]]);
        RFrame(offscreen.Canvas,xFeature-(9+1),yFeature+2+LinePitch*i-1,
          xFeature-(21-24),yFeature+2+LinePitch*i+12,
          MainTexture.clBevelShade,MainTexture.clBevelLight);
        end;

      // paint cost
      LightGradient(offscreen.Canvas,xFeature+34,yFeature+LinePitch*i,50,
        GrExt[HGrSystem].Data.Canvas.Pixels[187,137]);
      if (Domain=dGround) and (code[i]=mcDefense) then CapWeight:=2
      else CapWeight:=Feature[code[i]].Weight;
      n:=CapWeight+Feature[code[i]].Cost;
      d:=6;
      while (n-1)*d*2>48-10 do dec(d);
      for j:=0 to n-1 do
        if j<CapWeight then
          Sprite(offscreen,HGrSystem,xFeature+54+(j*2+1-n)*d,
            yFeature+2+LinePitch*i+1,10,10,88,126)
          else Sprite(offscreen,HGrSystem,xFeature+54+(j*2+1-n)*d,
            yFeature+2+LinePitch*i+1,10,10,88,115);
      end; // if not (code[i] in AutoFeature)
    DarkGradient(offscreen.Canvas,xFeature+17,yFeature+LinePitch*i,16,1);
    Frame(offscreen.canvas,xFeature+18,yFeature+1+LinePitch*i,
      xFeature+20-2+13,yFeature+2+1-2+13+LinePitch*i,$C0C0C0,$C0C0C0);
    Sprite(offscreen,HGrSystem,xFeature+20,yFeature+2+1+LinePitch*i,
      10,10,66+code[i] mod 11 *11,137+code[i] div 11 *11);

    if MyRO.DevModel.Cap[code[i]]>0 then TextColor:=MainTexture.clLitText
    else TextColor:=-1;

    if code[i]<mcFirstNonCap then
      LoweredTextOut(offscreen.Canvas,TextColor,MainTexture,xFeature+7,
        yFeature+LinePitch*i-1,IntToStr(MyRO.DevModel.Cap[code[i]]));
    LoweredTextOut(offscreen.Canvas,TextColor,MainTexture,xFeature+88,
      yFeature+LinePitch*i-1,Phrases.Lookup('FEATURES',code[i]));
    end;
  end;

with Tribe[me].ModelPicture[MyRO.nModel] do
  begin
  FrameImage(offscreen.canvas,BigImp,xView+4,yView+4,xSizeBig,ySizeBig,0,0);
  Sprite(offscreen,HGr,xView,yView,64,44,pix mod 10 *65+1,pix div 10*49+1);
  end;
MarkUsedOffscreen(InnerWidth,InnerHeight);
end;{MainPaint}

procedure TDraftDlg.SetDomain(d: integer);

  function Prio(fix: integer): integer;
  var
  FeaturePreq: integer;
  begin
  FeaturePreq:=Feature[fix].Preq;
  assert(FeaturePreq<>preNA);
  if fix<mcFirstNonCap then result:=10000+fix
  else if FeaturePreq=preNone then result:=20000
  else if FeaturePreq<0 then result:=40000
  else result:=30000+AdvValue[FeaturePreq];
  if not (fix in AutoFeature) then inc(result,90000);
  end;

var
i,j,x: integer;
begin
Domain:=d;
nAllCodes:=0;
for i:=0 to nFeature-1 do
  if (1 shl Domain and Feature[i].Domains<>0) and (Feature[i].Preq<>preNA)
    and ((Feature[i].Preq=preNone)
    or (Feature[i].Preq=preSun) and (MyRO.Wonder[woSun].EffectiveOwner=me)
    or (Feature[i].Preq>=0) and (MyRO.Tech[Feature[i].Preq]>=tsApplicable)) then
  begin AllCodes[nAllCodes]:=i; inc(nAllCodes) end;

// sort features
for i:=0 to nAllCodes-2 do for j:=i+1 to nAllCodes-1 do
  if Prio(AllCodes[i])>Prio(AllCodes[j]) then
    begin // exchange
    x:=AllCodes[i];
    AllCodes[i]:=AllCodes[j];
    AllCodes[j]:=x
    end;
end;

procedure TDraftDlg.FormShow(Sender: TObject);
begin
Domain:=dGround;
while (Domain<dAir) and (upgrade[Domain,0].Preq<>preNone)
  and (MyRO.Tech[upgrade[Domain,0].Preq]<tsApplicable) do inc(Domain);
SetDomain(Domain);
Server(sCreateDevModel,me,Domain,nil^);
MyModel[MyRO.nModel]:=MyRO.DevModel;
InitMyModel(MyRO.nModel,false);
sb.si.npos:=0;
OffscreenPaint;
InitPVSB(sb,Lines-1,VisibleLines);
IncCap:=-1; DecCap:=-1;
end;

procedure TDraftDlg.ShowNewContent(NewMode: integer);
begin
inherited ShowNewContent(NewMode);
end;

procedure TDraftDlg.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: integer);
var
i,d: integer;
begin
x:=x-SideFrame; y:=y-WideFrame;
if Button=mbLeft then
  begin
  for d:=0 to nDomains-1 do
    if (d<>Domain) and ((upgrade[d,0].Preq=preNone)
      or (MyRO.Tech[upgrade[d,0].Preq]>=tsApplicable))
      and (x>=xDomain) and (x<xDomain+36)
      and (y>=yDomain+d*DomainPitch) and (y<yDomain+d*DomainPitch+20) then
      begin
      SetDomain(d);
      Server(sCreateDevModel,me,Domain,nil^);
      MyModel[MyRO.nModel]:=MyRO.DevModel;
      InitMyModel(MyRO.nModel,false);
      sb.si.npos:=0;
      SmartUpdateContent;
      InitPVSB(sb,Lines-1,VisibleLines);
      end;

  if (y>=yFeature) and (y<yFeature+LinePitch*nCodes) then
    begin
    i:=(y-yFeature) div LinePitch;
    if (x>=xFeature-21) and (x<InnerWidth) and (ssShift in Shift) then
      HelpDlg.ShowNewContent(FWindowMode or wmPersistent, hkFeature, code[i])
    else if not (code[i] in AutoFeature) then
      begin
      if (code[i]<mcFirstNonCap) and (x>=xFeature-21) and (x<xFeature-21+12) then
        begin
        IncCap:=code[i];
        Dump(offscreen,HGrSystem,xFeature-21,yFeature+2+LinePitch*i,12,12,182,172);
        SmartInvalidate;
        end
      else if (x>=xFeature-9) and (x<xFeature-9+12) then
        begin
        DecCap:=code[i];
        if code[i]<mcFirstNonCap then
          Dump(offscreen,HGrSystem,xFeature-9,yFeature+2+LinePitch*i,12,12,182,159)
        else Dump(offscreen,HGrSystem,xFeature-9,yFeature+2+LinePitch*i,
          12,12,182,185+13*MyRO.DevModel.Cap[code[i]]);
        SmartInvalidate;
        end;
      end
    end
  end
end;

procedure TDraftDlg.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: integer);
var
NewValue: integer;
begin
if IncCap>=0 then
  begin
  NewValue:=MyRO.DevModel.Cap[IncCap]+1;
  Server(sSetDevModelCap+NewValue shl 4,me,IncCap,nil^);
  MyModel[MyRO.nModel]:=MyRO.DevModel;
  InitMyModel(MyRO.nModel,false);
  SmartUpdateContent;
  IncCap:=-1;
  end
else if DecCap>=0 then
  begin
  if (DecCap>=mcFirstNonCap) or (MyRO.DevModel.Cap[DecCap]>0) then
    begin
    NewValue:=MyRO.DevModel.Cap[DecCap]-1;
    if DecCap>=mcFirstNonCap then NewValue:=-NewValue;
    Server(sSetDevModelCap+NewValue shl 4,me,DecCap,nil^);
    MyModel[MyRO.nModel]:=MyRO.DevModel;
    InitMyModel(MyRO.nModel,false);
    end;
  SmartUpdateContent;
  DecCap:=-1;
  end;
end;

procedure TDraftDlg.OKBtnClick(Sender: TObject);
begin
ModalResult:=mrOK;
end;

end.

