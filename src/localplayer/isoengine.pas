{$INCLUDE switches.pas}

unit isoengine;

interface

uses
  Protocol, ClientTools, ScreenTools, Tribes,
  {$IFNDEF SCR}Term,{$ENDIF}

  LCLIntf, LCLType,
  SysUtils, Classes, Graphics;

type
  TInitEnemyModelEvent = function(emix: integer): boolean;

  TIsoMap = class
    constructor Create;
    procedure SetOutput(Output: TFPImageBitmap);
    procedure SetPaintBounds(Left, Top, Right, Bottom: integer);
    procedure Paint(x, y, Loc, nx, ny, CityLoc, CityOwner: integer;
      UseBlink: boolean = False; CityAllowClick: boolean = False);
    procedure PaintUnit(x, y: integer; const UnitInfo: TUnitInfo; Status: integer);
    procedure PaintCity(x, y: integer; const CityInfo: TCityInfo; accessory: boolean = True);
    procedure BitBlt_to_isoengine(Src: TFPImageBitmap; x, y, Width, Height, xSrc, ySrc, Rop: integer);

    procedure AttackBegin(const ShowMove: TShowMove);
    procedure AttackEffect(const ShowMove: TShowMove);
    procedure AttackEnd;

  protected
    FOutput: TFPImageBitmap;
    FLeft, FTop, FRight, FBottom, RealTop, RealBottom, AttLoc, DefLoc,
    DefHealth, FAdviceLoc: integer;
    function Connection4(Loc, Mask, Value: integer): integer;
    function Connection8(Loc, Mask: integer): integer;
    function OceanConnection(Loc: integer): integer;
    procedure PaintShore(x, y, Loc: integer);
    procedure PaintTileExtraTerrain(x, y, Loc: integer);
    procedure PaintTileObjects(x, y, Loc, CityLoc, CityOwner: integer; UseBlink: boolean);
    procedure PaintGrid(x, y, nx, ny: integer);
    procedure FillRect(x, y, w, h, Color: integer);
    procedure Textout(x, y, Color: integer; const s: string);
    procedure Sprite(HGr, xDst, yDst, Width, Height, xGr, yGr: integer);
    procedure TSprite(xDst, yDst, grix: integer; PureBlack: boolean = False);

  public
    property AdviceLoc: integer read FAdviceLoc write FAdviceLoc;
  end;


const
  // options switched by buttons
  moPolitical = 0;
  moCityNames = 1;
  moGreatWall = 4;
  moGrid = 5;
  moBareTerrain = 6;

  // other options
  moEditMode = 16;
  moLocCodes = 17;


var
  NoMap: TIsoMap;
  Options: integer;
  pDebugMap: integer; //-1 for off


function IsJungle(y: integer): boolean;
procedure Init(InitEnemyModelHandler: TInitEnemyModelEvent);
function ApplyTileSize(xxtNew, yytNew: integer): boolean;
procedure Done;
procedure Reset;


implementation

const
  ShoreDither = fGrass;
  TerrainIconLines = 21;

var
  BordersOK: integer;
  OnInitEnemyModel: TInitEnemyModelEvent;
  LandPatch, OceanPatch, Borders: TFPImageBitmap;
  TSpriteSize: array[0..TerrainIconLines * 9 - 1] of TRect;
  DebugMap: ^TTileList;
  CitiesPictures: array[2..3, 0..3] of TCityPicture;
  FoW, ShowLoc, ShowCityNames, ShowObjects, ShowBorder, ShowMyBorder,
  ShowGrWall, ShowDebug: boolean;

function IsJungle(y: integer): boolean;
begin
  Result := (y > (G.ly - 2) div 4) and (G.ly - 1 - y > (G.ly - 2) div 4);
end;

procedure Init(InitEnemyModelHandler: TInitEnemyModelEvent);
begin
  OnInitEnemyModel := InitEnemyModelHandler;
  if NoMap <> nil then
    NoMap.Free;
  NoMap := TIsoMap.Create;
end;

function ApplyTileSize(xxtNew, yytNew: integer): boolean;
type
  TLine = array[0..INFIN, 0..2] of byte;
var
  i, x, y, xSrc, ySrc, HGrTerrainNew, HGrCitiesNew, age, size: integer;
  LandMore, OceanMore, DitherMask, Mask24: TFPImageBitmap;
  MaskLine: array[0..32 * 3 - 1] of ^TLine; // 32 = assumed maximum for yyt
  Border: boolean;
begin
  Result := False;
  HGrTerrainNew := LoadGraphicSet('Terrain' + IntToStr(xxtNew * 2) +'x'+ IntToStr(yytNew * 2));
  if HGrTerrainNew < 0 then
    exit;
  HGrCitiesNew := LoadGraphicSet('Cities' + IntToStr(xxtNew * 2) +'x'+ IntToStr(yytNew * 2));
  if HGrCitiesNew < 0 then
    exit;
  xxt := xxtNew;
  yyt := yytNew;
  if yyt = 16 then begin
    citiesCurrent:=citiesSmall;
    terrainCurrent:=terrainSmall
  end else begin
    citiesCurrent:=citiesBig;
    terrainCurrent:=terrainBig;
  end;
  HGrTerrain := HGrTerrainNew;
  HGrCities := HGrCitiesNew;
  Result := True;

  // prepare age 2+3 cities
  for age := 2 to 3 do
    for size := 0 to 3 do
      with CitiesPictures[age, size] do
        FindPositionInPNG(citiesCurrent, size * (xxt * 2 + 1), (age - 2) * (yyt * 3 + 1), xxt * 2 - 1, yyt * 3 - 1,
          $00FFFF, xShield, yShield);

  {prepare dithered ground tiles}
  if LandPatch <> nil then
    LandPatch.Free;
  LandPatch := TPortableNetworkGraphic.Create;
  LandPatch.PixelFormat := pf24bit;
  LandPatch.Canvas.Brush.Color := 0;
  LandPatch.Width := xxt * 18;
  LandPatch.Height := yyt * 9;
  if OceanPatch <> nil then
    OceanPatch.Free;
  OceanPatch := TPortableNetworkGraphic.Create;
  OceanPatch.PixelFormat := pf24bit;
  OceanPatch.Canvas.Brush.Color := 0;
  OceanPatch.Width := xxt * 8;
  OceanPatch.Height := yyt * 4;
  LandMore := TPortableNetworkGraphic.Create;
  LandMore.PixelFormat := pf24bit;
  LandMore.Canvas.Brush.Color := 0;
  LandMore.Width := xxt * 18;
  LandMore.Height := yyt * 9;
  OceanMore := TPortableNetworkGraphic.Create;
  OceanMore.PixelFormat := pf24bit;
  OceanMore.Canvas.Brush.Color := 0;
  OceanMore.Width := xxt * 8;
  OceanMore.Height := yyt * 4;
  DitherMask := TPortableNetworkGraphic.Create;
  DitherMask.PixelFormat := pf24bit;
  DitherMask.Width := xxt * 2;
  DitherMask.Height := yyt * 2;
  BitBltUgly(DitherMask.Canvas.Handle, 0, 0, xxt * 2, yyt * 2,
    GrExt[HGrTerrain].Mask.Canvas.Handle, 1 + 7 * (xxt * 2 + 1), 1 + yyt + 15 * (yyt * 3 + 1), SRCAND);

  for x := -1 to 6 do
  begin
    if x = -1 then
    begin
      xSrc := ShoreDither * (xxt * 2 + 1) + 1;
      ySrc := 1 + yyt;
    end
    else if x = 6 then
    begin
      xSrc := 1 + (xxt * 2 + 1) * 2;
      ySrc := 1 + yyt + (yyt * 3 + 1) * 2;
    end
    else
    begin
      xSrc := (x + 2) * (xxt * 2 + 1) + 1;
      ySrc := 1 + yyt;
    end;
    for y := -1 to 6 do
      BitBltTransparent(LandPatch.Canvas, (x + 2) * (xxt * 2), (y + 2) * yyt, xxt * 2, yyt,
        xSrc, ySrc, terrainCurrent);
    for y := -2 to 6 do
      BitBltTransparent(LandPatch.Canvas, (x + 2) * (xxt * 2), (y + 2) * yyt, xxt, yyt,
        xSrc + xxt, ySrc + yyt, terrainCurrent);
    for y := -2 to 6 do
      BitBltTransparent(LandPatch.Canvas, (x + 2) * (xxt * 2) + xxt, (y + 2) * yyt, xxt, yyt,
        xSrc, ySrc + yyt, terrainCurrent);
    for y := -2 to 6 do
      BitBltUgly(LandPatch.Canvas.Handle, (x + 2) * (xxt * 2), (y + 2) * yyt, xxt, yyt,
        DitherMask.Canvas.Handle, xxt, yyt, SRCAND);
    for y := -2 to 6 do
      BitBltUgly(LandPatch.Canvas.Handle, (x + 2) * (xxt * 2) + xxt, (y + 2) * yyt, xxt, yyt,
        DitherMask.Canvas.Handle, 0, yyt, SRCAND);
  end;

  for y := -1 to 6 do
  begin
    if y = -1 then
    begin
      xSrc := ShoreDither * (xxt * 2 + 1) + 1;
      ySrc := 1 + yyt;
    end
    else if y = 6 then
    begin
      xSrc := 1 + 2 * (xxt * 2 + 1);
      ySrc := 1 + yyt + 2 * (yyt * 3 + 1);
    end
    else
    begin
      xSrc := (y + 2) * (xxt * 2 + 1) + 1;
      ySrc := 1 + yyt;
    end;
    for x := -2 to 6 do
      BitBltTransparent(LandMore.Canvas, (x + 2) * (xxt * 2), (y + 2) * yyt, xxt * 2, yyt,
        xSrc, ySrc, terrainCurrent);
    BitBltTransparent(LandMore.Canvas, xxt * 2, (y + 2) * yyt, xxt, yyt,
      xSrc + xxt, ySrc + yyt, terrainCurrent);
    for x := 0 to 7 do
      BitBltTransparent(LandMore.Canvas, (x + 2) * (xxt * 2) - xxt, (y + 2) * yyt, xxt * 2, yyt,
        xSrc, ySrc + yyt, terrainCurrent);
    for x := -2 to 6 do
      BitBltUgly(LandMore.Canvas.Handle, (x + 2) * (xxt * 2), (y + 2) * yyt, xxt * 2, yyt,
        DitherMask.Canvas.Handle, 0, 0, SRCAND);
  end;

  for x := 0 to 3 do
    for y := 0 to 3 do
    begin
      if (x = 1) and (y = 1) then
        xSrc := 1
      else
        xSrc := (x mod 2) * (xxt * 2 + 1) + 1;
      ySrc := 1 + yyt;
      if (x >= 1) = (y >= 2) then
        BitBltTransparent(OceanPatch.Canvas, x * (xxt * 2), y * yyt, xxt * 2, yyt,
          xSrc, ySrc, terrainCurrent);
      if (x >= 1) and ((y < 2) or (x >= 2)) then
      begin
        BitBltTransparent(OceanPatch.Canvas, x * (xxt * 2), y * yyt, xxt, yyt, xSrc + xxt, ySrc + yyt, terrainCurrent);
        BitBltTransparent(OceanPatch.Canvas, x * (xxt * 2) + xxt, y * yyt, xxt, yyt, xSrc, ySrc + yyt, terrainCurrent);
      end;
      BitBltUgly(OceanPatch.Canvas.Handle, x * (xxt * 2), y * yyt, xxt, yyt,
        DitherMask.Canvas.Handle, xxt, yyt, SRCAND);
      BitBltUgly(OceanPatch.Canvas.Handle, x * (xxt * 2) + xxt, y * yyt, xxt, yyt,
        DitherMask.Canvas.Handle, 0, yyt, SRCAND);
    end;

  for y := 0 to 3 do
    for x := 0 to 3 do
    begin
      if (x = 1) and (y = 1) then
        xSrc := 1
      else
        xSrc := (y mod 2) * (xxt * 2 + 1) + 1;
      ySrc := 1 + yyt;
      if (x < 1) or (y >= 2) then
        BitBltTransparent(OceanMore.Canvas, x * (xxt * 2), y * yyt, xxt * 2, yyt,
          xSrc, ySrc, terrainCurrent);
      if (x = 1) and (y < 2) or (x >= 2) and (y >= 1) then
      begin
        BitBltTransparent(OceanMore.Canvas, x * (xxt * 2), y * yyt, xxt, yyt,
          xSrc + xxt, ySrc + yyt, terrainCurrent);
        BitBltTransparent(OceanMore.Canvas, x * (xxt * 2) + xxt, y * yyt, xxt, yyt,
          xSrc, ySrc + yyt, terrainCurrent);
      end;
      BitBltUgly(OceanMore.Canvas.Handle, x * (xxt * 2), y * yyt, xxt * 2, yyt,
        DitherMask.Canvas.Handle, 0, 0, SRCAND);
    end;

  BitBltUgly(DitherMask.Canvas.Handle, 0, 0, xxt * 2, yyt * 2,
    DitherMask.Canvas.Handle, 0, 0, DSTINVERT); {invert dither mask}
  BitBltUgly(DitherMask.Canvas.Handle, 0, 0, xxt * 2, yyt * 2,
    GrExt[HGrTerrain].Mask.Canvas.Handle, 1, 1 + yyt, SRCPAINT);

  for x := -1 to 6 do
    for y := -2 to 6 do
      BitBltUgly(LandPatch.Canvas.Handle, (x + 2) * (xxt * 2), (y + 2) * yyt, xxt * 2, yyt,
        DitherMask.Canvas.Handle, 0, 0, SRCAND);

  for y := -1 to 6 do
    for x := -2 to 7 do
      BitBltUgly(LandMore.Canvas.Handle, (x + 2) * (xxt * 2) - xxt, (y + 2) * yyt, xxt * 2, yyt,
        DitherMask.Canvas.Handle, 0, yyt, SRCAND);

  BitBltUgly(LandPatch.Canvas.Handle, 0, 0, (xxt * 2) * 9, yyt * 9, LandMore.Canvas.Handle, 0, 0,
    SRCPAINT);

  for x := 0 to 3 do
    for y := 0 to 3 do
      BitBltUgly(OceanPatch.Canvas.Handle, x * (xxt * 2), y * yyt, xxt * 2, yyt,
        DitherMask.Canvas.Handle, 0, 0, SRCAND);

  for y := 0 to 3 do
    for x := 0 to 4 do
      BitBltUgly(OceanMore.Canvas.Handle, x * (xxt * 2) - xxt, y * yyt, xxt * 2, yyt,
        DitherMask.Canvas.Handle, 0, yyt, SRCAND);

  BitBltUgly(OceanPatch.Canvas.Handle, 0, 0, (xxt * 2) * 4, yyt * 4, OceanMore.Canvas.Handle, 0, 0,
    SRCPAINT);

  with DitherMask.Canvas do
  begin
    Brush.Color := $FFFFFF;
    FillRect(Rect(0, 0, xxt * 2, yyt));
  end;
  BitBltUgly(DitherMask.Canvas.Handle, 0, 0, xxt * 2, yyt,
    GrExt[HGrTerrain].Mask.Canvas.Handle, 1, 1 + yyt, SRCCOPY);

  for x := 0 to 6 do
    BitBltUgly(LandPatch.Canvas.Handle, (x + 2) * (xxt * 2), yyt, xxt * 2, yyt,
      DitherMask.Canvas.Handle, 0, 0, SRCAND);

  BitBltUgly(DitherMask.Canvas.Handle, 0, 0, xxt * 2, yyt,
    DitherMask.Canvas.Handle, 0, 0, DSTINVERT);

  for y := 0 to 6 do
    BitBltUgly(LandPatch.Canvas.Handle, xxt * 2, (y + 2) * yyt, xxt * 2, yyt,
      DitherMask.Canvas.Handle, 0, 0, SRCAND);

  LandMore.Free;
  OceanMore.Free;
  DitherMask.Free;
  //LandPatch.Savetofile('landpatch.bmp');

  // reduce size of terrain icons
  Mask24 := TPortableNetworkGraphic.Create;
  Mask24.Assign(GrExt[HGrTerrain].Mask);
  Mask24.PixelFormat := pf24bit;
  for ySrc := 0 to TerrainIconLines - 1 do
  begin
    for i := 0 to yyt * 3 - 1 do
    begin
      Mask24.BeginUpdate();
      MaskLine[i] := Mask24.ScanLine[1 + ySrc * (yyt * 3 + 1) + i];
      Mask24.EndUpdate();
    end;
    for xSrc := 0 to 9 - 1 do
    begin
      i := ySrc * 9 + xSrc;
      TSpriteSize[i].Left := 0;
      repeat
        Border := True;
        for y := 0 to yyt * 3 - 1 do
          if MaskLine[y]^[1 + xSrc * (xxt * 2 + 1) + TSpriteSize[i].Left, 0] = 0 then
            Border := False;
        if Border then
          Inc(TSpriteSize[i].Left)
      until not Border or (TSpriteSize[i].Left = xxt * 2 - 1);
      TSpriteSize[i].Top := 0;
      repeat
        Border := True;
        for x := 0 to xxt * 2 - 1 do
          if MaskLine[TSpriteSize[i].Top]^[1 + xSrc * (xxt * 2 + 1) + x, 0] = 0 then
            Border := False;
        if Border then
          Inc(TSpriteSize[i].Top)
      until not Border or (TSpriteSize[i].Top = yyt * 3 - 1);
      TSpriteSize[i].Right := xxt * 2;
      repeat
        Border := True;
        for y := 0 to yyt * 3 - 1 do
          if MaskLine[y]^[xSrc * (xxt * 2 + 1) + TSpriteSize[i].Right, 0] = 0 then
            Border := False;
        if Border then
          Dec(TSpriteSize[i].Right)
      until not Border or (TSpriteSize[i].Right = TSpriteSize[i].Left);
      TSpriteSize[i].Bottom := yyt * 3;
      repeat
        Border := True;
        for x := 0 to xxt * 2 - 1 do
          if MaskLine[TSpriteSize[i].Bottom - 1]^[1 + xSrc * (xxt * 2 + 1) + x, 0] = 0 then
            Border := False;
        if Border then
          Dec(TSpriteSize[i].Bottom)
      until not Border or (TSpriteSize[i].Bottom = TSpriteSize[i].Top);
    end;
  end;
  Mask24.Free;

  if Borders <> nil then
    Borders.Free;
  Borders := TPortableNetworkGraphic.Create;
  Borders.PixelFormat := pf24bit;
  Borders.Width := xxt * 2;
  Borders.Height := (yyt * 2) * nPl;
  BordersOK := 0;
end;

procedure Done;
begin
  NoMap.Free;
  NoMap := nil;
  LandPatch.Free;
  LandPatch := nil;
  OceanPatch.Free;
  OceanPatch := nil;
  Borders.Free;
  Borders := nil;
end;

procedure Reset;
begin
  BordersOK := 0;
end;

constructor TIsoMap.Create;
begin
  inherited;
  FLeft := 0;
  FTop := 0;
  FRight := 0;
  FBottom := 0;
  AttLoc := -1;
  DefLoc := -1;
  FAdviceLoc := -1;
end;

procedure TIsoMap.SetOutput(Output: TFPImageBitmap);
begin
  FOutput := Output;
  FLeft := 0;
  FTop := 0;
  FRight := FOutput.Width;
  FBottom := FOutput.Height;
end;

procedure TIsoMap.SetPaintBounds(Left, Top, Right, Bottom: integer);
begin
  FLeft := Left;
  FTop := Top;
  FRight := Right;
  FBottom := Bottom;
end;

procedure TIsoMap.FillRect(x, y, w, h, Color: integer);
begin
  if x < FLeft then
  begin
    w := w - (FLeft - x);
    x := FLeft;
  end;
  if y < FTop then
  begin
    h := h - (FTop - y);
    y := FTop;
  end;
  if x + w >= FRight then
    w := FRight - x;
  if y + h >= FBottom then
    h := FBottom - y;
  if (w <= 0) or (h <= 0) then
    exit;

  with FOutput.Canvas do
  begin
    Brush.Color := Color;
    FillRect(Rect(x, y, x + w, y + h));
    Brush.Style := bsClear;
  end;
end;

procedure TIsoMap.Textout(x, y, Color: integer; const s: string);
begin
  FOutput.Canvas.Font.Color := Color;
  FOutput.Canvas.TextRect(Rect(FLeft, FTop, FRight, FBottom), x, y, s);
end;

procedure TIsoMap.BitBlt_to_isoengine(Src: TFPImageBitmap; x, y, Width, Height, xSrc, ySrc, Rop: integer);
begin
  if x < FLeft then
  begin
    Width := Width - (FLeft - x);
    xSrc := xSrc + (FLeft - x);
    x := FLeft;
  end;
  if y < FTop then
  begin
    Height := Height - (FTop - y);
    ySrc := ySrc + (FTop - y);
    y := FTop;
  end;
  if x + Width >= FRight then
    Width := FRight - x;
  if y + Height >= FBottom then
    Height := FBottom - y;
  if (Width <= 0) or (Height <= 0) then
    exit;

  BitBltUgly(FOutput.Canvas.Handle, x, y, Width, Height, Src.Canvas.Handle, xSrc,
    ySrc, Rop);
end;

procedure TIsoMap.Sprite(HGr, xDst, yDst, Width, Height, xGr, yGr: integer);
begin
  BitBlt_to_isoengine(GrExt[HGr].Mask, xDst, yDst, Width, Height, xGr, yGr, SRCAND);
  BitBlt_to_isoengine(GrExt[HGr].Data, xDst, yDst, Width, Height, xGr, yGr, SRCPAINT);
end;

procedure TIsoMap.TSprite(xDst, yDst, grix: integer; PureBlack: boolean = False);
var
  Width, Height, xSrc, ySrc: integer;
begin
  Width := TSpriteSize[grix].Right - TSpriteSize[grix].Left;
  Height := TSpriteSize[grix].Bottom - TSpriteSize[grix].Top;
  xSrc := 1 + grix mod 9 * (xxt * 2 + 1) + TSpriteSize[grix].Left;
  ySrc := 1 + grix div 9 * (yyt * 3 + 1) + TSpriteSize[grix].Top;
  xDst := xDst + TSpriteSize[grix].Left;
  yDst := yDst - yyt + TSpriteSize[grix].Top;
  if xDst < FLeft then
  begin
    Width := Width - (FLeft - xDst);
    xSrc := xSrc + (FLeft - xDst);
    xDst := FLeft;
  end;
  if yDst < FTop then
  begin
    Height := Height - (FTop - yDst);
    ySrc := ySrc + (FTop - yDst);
    yDst := FTop;
  end;
  if xDst + Width >= FRight then
    Width := FRight - xDst;
  if yDst + Height >= FBottom then
    Height := FBottom - yDst;
  if (Width <= 0) or (Height <= 0) then
    exit;

  // lazarus TODO: get rid of this `PureBlack` totally?
  if PureBlack then
    BitBltUgly(FOutput.Canvas.Handle, xDst, yDst, Width, Height, terrainCurrent.Canvas.Handle, xSrc, ySrc, SRCAND)
  else
    BitBltTransparent(FOutput.Canvas, xDst, yDst, Width, Height, xSrc, ySrc, terrainCurrent);
end;

procedure TIsoMap.PaintUnit(x, y: integer; const UnitInfo: TUnitInfo; Status: integer);
var
  xsh, ysh, xGr, yGr, j, mixShow: integer;
begin
  with UnitInfo do
    if (Owner = me) or (emix <> $FFFF) then
    begin
      if Job = jCity then
        mixShow := -1 // building site
      else
        mixShow := mix;
      if (Tribe[Owner].ModelPicture[mixShow].HGr = 0) and (@OnInitEnemyModel <> nil) then
        if not OnInitEnemyModel(emix) then
          exit;
      xsh := Tribe[Owner].ModelPicture[mixShow].xShield;
      ysh := Tribe[Owner].ModelPicture[mixShow].yShield;
  {$IFNDEF SCR}
      if Status and usStay <> 0 then
        j := 19
      else if Status and usRecover <> 0 then
        j := 16
      else if Status and (usGoto or usEnhance) = usGoto or usEnhance then
        j := 18
      else if Status and usEnhance <> 0 then
        j := 17
      else if Status and usGoto <> 0 then
        j := 20
      else
{$ENDIF}
      if Job = jCity then
        j := jNone
      else
        j := Job;
      if Flags and unMulti <> 0 then
        BitBltTransparent(FOutput.Canvas, x + xsh - 1 + 4, y + ysh - 2, 14, 12,
          33 + Tribe[Owner].sympix mod 10 * 65, 1 + Tribe[Owner].sympix div 10 * 49, Tribe[Owner].symPNG);
      BitBltTransparent(FOutput.Canvas, x + xsh - 1, y + ysh - 2, 14, 12,
        18 + Tribe[Owner].sympix mod 10 * 65, 1 + Tribe[Owner].sympix div 10 * 49, Tribe[Owner].symPNG);
      FillRect(x + xsh, y + ysh + 5, 1 + Health * 11 div 100, 3, ColorOfHealth(Health));
      if j > 0 then
      begin
        xGr := 121 + j mod 7 * 9;
        yGr := 1 + j div 7 * 9;
        BitBltTransparent(FOutput.Canvas, x + xsh + 2, y + ysh + 8, 8, 8, xGr, yGr, system1transparent);
      end;
      with Tribe[Owner].ModelPicture[mixShow] do
        Sprite(HGr, x, y, 64, 48, pix mod 10 * 65 + 1, pix div 10 * 49 + 1);
      if Flags and unFortified <> 0 then
      begin
{
    TSprite(x,y+16,12*9+7);}
        BitBltTransparent(FOutput.Canvas, x, y, xxu * 2, yyu * 2, 1 + 6 * (xxu * 2 + 1), 1, stdUnitsPng);
      end;
    end;
end;{PaintUnit}

procedure TIsoMap.PaintCity(x, y: integer; const CityInfo: TCityInfo; accessory: boolean);
var
  age, cpix, xGr, xShield, yShield, LabelTextColor, LabelLength: integer;
  cpic: TCityPicture;
  s: string;
begin
  age := GetAge(CityInfo.Owner);
  if CityInfo.Size < 5 then
    xGr := 0
  else if CityInfo.Size < 9 then
    xGr := 1
  else if CityInfo.Size < 13 then
    xGr := 2
  else
    xGr := 3;
  Tribe[CityInfo.Owner].InitAge(age);
  if age < 2 then
  begin
    cpix := Tribe[CityInfo.Owner].cpix;
    if (ciWalled and CityInfo.Flags = 0) or
      (Tribe[CityInfo.Owner].cityPNG.Canvas.Pixels[(xGr + 4) * 65, cpix * 49 + 48] = $00FFFF) then
      BitBltTransparent(FOutput.Canvas, x - xxc, y - 2 * yyc, xxc * 2, yyc * 3, xGr * (xxc * 2 + 1) + 1, 1 + cpix * (yyc * 3 + 1), Tribe[CityInfo.Owner].cityPNG);
    if ciWalled and CityInfo.Flags <> 0 then
      BitBltTransparent(FOutput.Canvas, x - xxc, y - 2 * yyc, xxc * 2, yyc * 3, (xGr + 4) * (xxc * 2 + 1) + 1, 1 + cpix * (yyc * 3 + 1), Tribe[CityInfo.Owner].cityPNG);
  end
  else
  begin
    if ciWalled and CityInfo.Flags <> 0 then
      BitBltTransparent(FOutput.Canvas, x - xxt, y - 2 * yyt, 2 * xxt, 3 * yyt, (xGr + 4) * (2 * xxt + 1) + 1, 1 + (age - 2) * (3 * yyt + 1), citiesCurrent)
    else
      BitBltTransparent(FOutput.Canvas, x - xxt, y - 2 * yyt, 2 * xxt, 3 * yyt, xGr * (2 * xxt + 1) + 1, 1 + (age - 2) * (3 * yyt + 1), citiesCurrent);
  end;

  if not Accessory then
    exit;

  if MyMap[CityInfo.Loc] and fObserved <> 0 then
  begin
    if age < 2 then
    begin
      cpic := Tribe[CityInfo.Owner].CityPicture[xGr];
      xShield := x - xxc + cpic.xShield;
      yShield := y - 2 * yyc + cpic.yShield;
    end
    else
    begin
      cpic := CitiesPictures[age, xGr];
      xShield := x - xxt + cpic.xShield;
      yShield := y - 2 * yyt + cpic.yShield;
    end;
    s := IntToStr(CityInfo.Size);
    LabelLength := FOutput.Canvas.TextWidth(s);
    FillRect(xShield, yShield, LabelLength + 4, 16, $000000);
    if MyMap[CityInfo.Loc] and (fUnit or fObserved) = fObserved then
      // empty city
      LabelTextColor := Tribe[CityInfo.Owner].Color
    else
    begin
      FillRect(xShield + 1, yShield + 1, LabelLength + 2, 14, Tribe[CityInfo.Owner].Color);
      LabelTextColor := $000000;
    end;
    Textout(xShield + 2, yShield - 1, LabelTextColor, s);
  end;
end;{PaintCity}

function PoleTile(Loc: integer): integer;
begin {virtual pole tile}
  Result := fUNKNOWN;
  if Loc < -2 * G.lx then
  else if Loc < -G.lx then
  begin
    if (MyMap[dLoc(Loc, 0, 2)] and fTerrain <> fUNKNOWN) and
      (MyMap[dLoc(Loc, -2, 2)] and fTerrain <> fUNKNOWN) and
      (MyMap[dLoc(Loc, 2, 2)] and fTerrain <> fUNKNOWN) then
      Result := fArctic;
    if (MyMap[dLoc(Loc, 0, 2)] and fObserved <> 0) and
      (MyMap[dLoc(Loc, -2, 2)] and fObserved <> 0) and
      (MyMap[dLoc(Loc, 2, 2)] and fObserved <> 0) then
      Result := Result or fObserved;
  end
  else if Loc < 0 then
  begin
    if (MyMap[dLoc(Loc, -1, 1)] and fTerrain <> fUNKNOWN) and
      (MyMap[dLoc(Loc, 1, 1)] and fTerrain <> fUNKNOWN) then
      Result := fArctic;
    if (MyMap[dLoc(Loc, -1, 1)] and fObserved <> 0) and
      (MyMap[dLoc(Loc, 1, 1)] and fObserved <> 0) then
      Result := Result or fObserved;
  end
  else if Loc < G.lx * (G.ly + 1) then
  begin
    if (MyMap[dLoc(Loc, -1, -1)] and fTerrain <> fUNKNOWN) and
      (MyMap[dLoc(Loc, 1, -1)] and fTerrain <> fUNKNOWN) then
      Result := fArctic;
    if (MyMap[dLoc(Loc, -1, -1)] and fObserved <> 0) and
      (MyMap[dLoc(Loc, 1, -1)] and fObserved <> 0) then
      Result := Result or fObserved;
  end
  else if Loc < G.lx * (G.ly + 2) then
  begin
    if (MyMap[dLoc(Loc, 0, -2)] and fTerrain <> fUNKNOWN) and
      (MyMap[dLoc(Loc, -2, -2)] and fTerrain <> fUNKNOWN) and
      (MyMap[dLoc(Loc, 2, -2)] and fTerrain <> fUNKNOWN) then
      Result := fArctic;
    if (MyMap[dLoc(Loc, 0, -2)] and fObserved <> 0) and
      (MyMap[dLoc(Loc, -2, -2)] and fObserved <> 0) and
      (MyMap[dLoc(Loc, 2, -2)] and fObserved <> 0) then
      Result := Result or fObserved;
  end;
end;

const
  Dirx: array[0..7] of integer = (1, 2, 1, 0, -1, -2, -1, 0);
  Diry: array[0..7] of integer = (-1, 0, 1, 2, 1, 0, -1, -2);

function TIsoMap.Connection4(Loc, Mask, Value: integer): integer;
begin
  Result := 0;
  if dLoc(Loc, 1, -1) >= 0 then
  begin
    if MyMap[dLoc(Loc, 1, -1)] and Mask = cardinal(Value) then
      Inc(Result, 1);
    if MyMap[dLoc(Loc, -1, -1)] and Mask = cardinal(Value) then
      Inc(Result, 8);
  end;
  if dLoc(Loc, 1, 1) < G.lx * G.ly then
  begin
    if MyMap[dLoc(Loc, 1, 1)] and Mask = cardinal(Value) then
      Inc(Result, 2);
    if MyMap[dLoc(Loc, -1, 1)] and Mask = cardinal(Value) then
      Inc(Result, 4);
  end;
end;

function TIsoMap.Connection8(Loc, Mask: integer): integer;
var
  Dir, ConnLoc: integer;
begin
  Result := 0;
  for Dir := 0 to 7 do
  begin
    ConnLoc := dLoc(Loc, Dirx[Dir], Diry[Dir]);
    if (ConnLoc >= 0) and (ConnLoc < G.lx * G.ly) and (MyMap[ConnLoc] and Mask <> 0) then
      Inc(Result, 1 shl Dir);
  end;
end;

function TIsoMap.OceanConnection(Loc: integer): integer;
var
  Dir, ConnLoc: integer;
begin
  Result := 0;
  for Dir := 0 to 7 do
  begin
    ConnLoc := dLoc(Loc, Dirx[Dir], Diry[Dir]);
    if (ConnLoc < 0) or (ConnLoc >= G.lx * G.ly) or ((MyMap[ConnLoc] - 2) and fTerrain < 13) then
      Inc(Result, 1 shl Dir);
  end;
end;

procedure TIsoMap.PaintShore(x, y, Loc: integer);
var
  Conn, Tile: integer;
begin
  if (y <= FTop - yyt * 2) or (y > FBottom) or (x <= FLeft - xxt * 2) or (x > FRight) then
    exit;
  if (Loc < 0) or (Loc >= G.lx * G.ly) then
    exit;
  Tile := MyMap[Loc];
  if Tile and fTerrain >= fGrass then
    exit;
  Conn := OceanConnection(Loc);
  if Conn = 0 then
    exit;

  BitBlt_to_isoengine(GrExt[HGrTerrain].Data, x + xxt div 2, y, xxt, yyt,
    1 + (Conn shr 6 + Conn and 1 shl 2) * (xxt * 2 + 1),
    1 + yyt + (16 + Tile and fTerrain) * (yyt * 3 + 1), SRCPAINT);
  BitBlt_to_isoengine(GrExt[HGrTerrain].Data, x + xxt, y + yyt div 2, xxt, yyt,
    1 + (Conn and 7) * (xxt * 2 + 1) + xxt,
    1 + yyt * 2 + (16 + Tile and fTerrain) * (yyt * 3 + 1), SRCPAINT);
  BitBlt_to_isoengine(GrExt[HGrTerrain].Data, x + xxt div 2, y + yyt, xxt, yyt,
    1 + (Conn shr 2 and 7) * (xxt * 2 + 1) + xxt,
    1 + yyt + (16 + Tile and fTerrain) * (yyt * 3 + 1), SRCPAINT);
  BitBlt_to_isoengine(GrExt[HGrTerrain].Data, x, y + yyt div 2, xxt, yyt,
    1 + (Conn shr 4 and 7) * (xxt * 2 + 1),
    1 + yyt * 2 + (16 + Tile and fTerrain) * (yyt * 3 + 1), SRCPAINT);
  Conn := Connection4(Loc, fTerrain, fUNKNOWN); {dither to black}
  if Conn and 1 <> 0 then
    BitBlt_to_isoengine(GrExt[HGrTerrain].Mask, x + xxt, y, xxt, yyt, 1 + 7 * (xxt * 2 + 1) + xxt,
      1 + yyt + 15 * (yyt * 3 + 1), SRCAND);
  if Conn and 2 <> 0 then
    BitBlt_to_isoengine(GrExt[HGrTerrain].Mask, x + xxt, y + yyt, xxt, yyt, 1 + 7 * (xxt * 2 + 1) + xxt,
      1 + yyt * 2 + 15 * (yyt * 3 + 1), SRCAND);
  if Conn and 4 <> 0 then
    BitBlt_to_isoengine(GrExt[HGrTerrain].Mask, x, y + yyt, xxt, yyt, 1 + 7 * (xxt * 2 + 1),
      1 + yyt * 2 + 15 * (yyt * 3 + 1), SRCAND);
  if Conn and 8 <> 0 then
    BitBlt_to_isoengine(GrExt[HGrTerrain].Mask, x, y, xxt, yyt, 1 + 7 * (xxt * 2 + 1),
      1 + yyt + 15 * (yyt * 3 + 1), SRCAND);
end;

procedure TIsoMap.PaintTileExtraTerrain(x, y, Loc: integer);
var
  Dir, Conn, RRConn, yGr, Tile, yLoc: integer;
begin
  if (Loc < 0) or (Loc >= G.lx * G.ly) or (y <= -yyt * 2) or (y > FOutput.Height) or
    (x <= -xxt * 2) or (x > FOutput.Width) then
    exit;
  Tile := MyMap[Loc];
  if Tile and fTerrain = fForest then
  begin
    yLoc := Loc div G.lx;
    if IsJungle(yLoc) then
      yGr := 18
    else
      yGr := 3;
    Conn := Connection4(Loc, fTerrain, Tile and fTerrain);
    if (yLoc = (G.ly - 2) div 4) or (G.ly - 1 - yLoc = (G.ly + 2) div 4) then
      Conn := Conn and not 6 // no connection to south
    else if (yLoc = (G.ly + 2) div 4) or (G.ly - 1 - yLoc = (G.ly - 2) div 4) then
      Conn := Conn and not 9; // no connection to north
    TSprite(x, y, Conn mod 8 + (yGr + Conn div 8) * 9);
  end
  else if Tile and fTerrain in [fHills, fMountains, fForest] then
  begin
    yGr := 3 + 2 * (Tile and fTerrain - fForest);
    Conn := Connection4(Loc, fTerrain, Tile and fTerrain);
    TSprite(x, y, Conn mod 8 + (yGr + Conn div 8) * 9);
  end
  else if Tile and fDeadLands <> 0 then
    TSprite(x, y, 2 * 9 + 6);

  if ShowObjects then
  begin
    if Tile and fTerImp = tiFarm then
      TSprite(x, y, 109) {farmland}
    else if Tile and fTerImp = tiIrrigation then
      TSprite(x, y, 108); // irrigation
  end;
  if Tile and fRiver <> 0 then
  begin
    Conn := Connection4(Loc, fRiver, fRiver) or Connection4(Loc, fTerrain, fShore) or
      Connection4(Loc, fTerrain, fUNKNOWN);
    TSprite(x, y, Conn mod 8 + (13 + Conn div 8) * 9);
  end;

  if Tile and fTerrain < fGrass then
  begin
    Conn := Connection4(Loc, fRiver, fRiver);
    for Dir := 0 to 3 do
      if Conn and (1 shl Dir) <> 0 then {river mouths}
        TSprite(x, y, 15 * 9 + Dir);
    if ShowObjects then
    begin
      Conn := Connection8(Loc, fCanal);
      for Dir := 0 to 7 do
        if Conn and (1 shl Dir) <> 0 then {canal mouths}
          TSprite(x, y, 20 * 9 + 1 + Dir);
    end;
  end;

  if ShowObjects then
  begin
    if (Tile and fCanal <> 0) or (Tile and fCity <> 0) then
    begin // paint canal connections
      Conn := Connection8(Loc, fCanal or fCity);
      if Tile and fCanal <> 0 then
        Conn := Conn or ($FF - OceanConnection(Loc));
      if Conn = 0 then
      begin
        if Tile and fCanal <> 0 then
          TSprite(x, y, 99);
      end
      else
        for Dir := 0 to 7 do
          if (1 shl Dir) and Conn <> 0 then
            TSprite(x, y, 100 + Dir);
    end;
    if Tile and (fRR or fCity) <> 0 then
      RRConn := Connection8(Loc, fRR or fCity)
    else
      RRConn := 0;
    if Tile and (fRoad or fRR or fCity) <> 0 then
    begin // paint road connections
      Conn := Connection8(Loc, fRoad or fRR or fCity) and not RRConn;
      if (Conn = 0) and (Tile and (fRR or fCity) = 0) then
        TSprite(x, y, 81)
      else if Conn > 0 then
        for Dir := 0 to 7 do
          if (1 shl Dir) and Conn <> 0 then
            TSprite(x, y, 82 + Dir);
    end;
    // paint railroad connections
    if (Tile and fRR <> 0) and (RRConn = 0) then
      TSprite(x, y, 90)
    else if RRConn > 0 then
      for Dir := 0 to 7 do
        if (1 shl Dir) and RRConn <> 0 then
          TSprite(x, y, 91 + Dir);
  end;
end;

// (x,y) is top left pixel of (2*xxt,3*yyt) rectangle
procedure TIsoMap.PaintTileObjects(x, y, Loc, CityLoc, CityOwner: integer;
  UseBlink: boolean);
type
  TLine = array[0..9 * 65, 0..2] of byte;
var
  p1, p2, uix, cix, dy, Loc1, Tile, Multi, Destination: integer;
  CityInfo: TCityInfo;
  UnitInfo: TUnitInfo;
  fog: boolean;

  procedure NameCity;
  var
    cix, xs, w: integer;
    BehindCityInfo: TCityInfo;
    s: string;
    IsCapital: boolean;
  begin
    BehindCityInfo.Loc := Loc - 2 * G.lx;
    if ShowCityNames and (Options and (1 shl moEditMode) = 0) and
      (BehindCityInfo.Loc >= 0) and (BehindCityInfo.Loc < G.lx * G.ly) and
      (MyMap[BehindCityInfo.Loc] and fCity <> 0) then
    begin
      GetCityInfo(BehindCityInfo.Loc, cix, BehindCityInfo);
      IsCapital := BehindCityInfo.Flags and ciCapital <> 0;
    {if Showuix and (cix>=0) then s:=IntToStr(cix)
    else} s := CityName(BehindCityInfo.ID);
      w := FOutput.Canvas.TextWidth(s);
      xs := x + xxt - (w + 1) div 2;
      if IsCapital then
        FOutput.Canvas.Font.Style := FOutput.Canvas.Font.Style + [fsUnderline];
      Textout(xs + 1, y - 9, $000000, s);
      Textout(xs, y - 10, $FFFFFF, s);
      if IsCapital then
        FOutput.Canvas.Font.Style := FOutput.Canvas.Font.Style - [fsUnderline];
    end;
  end;

  procedure ShowSpacePort;
  begin
    if ShowObjects and (Options and (1 shl moEditMode) = 0) and
      (Tile and fCity <> 0) and (CityInfo.Flags and ciSpacePort <> 0) then
      TSprite(x + xxt, y - 6, 12 * 9 + 5);
  end;

  procedure PaintBorder;
  var
    dx, dy: integer;
    Line: ^TLine;
  begin
    if ShowBorder and (Loc >= 0) and (Loc < G.lx * G.ly) and
      (Tile and fTerrain <> fUNKNOWN) then
    begin
      p1 := MyRO.Territory[Loc];
      if (p1 >= 0) and (ShowMyBorder or (p1 <> me)) then
      begin
        if BordersOK and (1 shl p1) = 0 then
        begin
          BitBltTransparent(Borders.Canvas, 0, p1 * (yyt * 2), xxt * 2, yyt * 2,
            1 + 8 * (xxt * 2 + 1), 1 + yyt + 16 * (yyt * 3 + 1), terrainCurrent);
          for dy := 0 to yyt * 2 - 1 do
          begin
            Borders.BeginUpdate();
            Line := Borders.ScanLine[p1 * (yyt * 2) + dy];
            Borders.EndUpdate();
            for dx := 0 to xxt * 2 - 1 do
              if Line[dx, 0] = 99 then
              begin
                Line[dx, 0] := Tribe[p1].Color shr 16 and $FF;
                Line[dx, 1] := Tribe[p1].Color shr 8 and $FF;
                Line[dx, 2] := Tribe[p1].Color and $FF;
              end;
          end;
          BordersOK := BordersOK or 1 shl p1;
        end;
        for dy := 0 to 1 do
          for dx := 0 to 1 do
          begin
            Loc1 := dLoc(Loc, dx * 2 - 1, dy * 2 - 1);
            begin
              if (Loc1 < 0) or (Loc1 >= G.lx * G.ly) then
                p2 := -1
              else if MyMap[Loc1] and fTerrain = fUNKNOWN then
                p2 := p1
              else
                p2 := MyRO.Territory[Loc1];
              if p2 <> p1 then
              begin
                BitBlt_to_isoengine(GrExt[HGrTerrain].Mask, x + dx * xxt, y + dy * yyt, xxt, yyt, 1 + 8 * (xxt * 2 + 1) + dx * xxt, 1 + yyt + 16 * (yyt * 3 + 1) + dy * yyt, SRCAND);
                BitBlt_to_isoengine(Borders,                x + dx * xxt, y + dy * yyt, xxt, yyt, dx * xxt, p1 * (yyt * 2) + dy * yyt, SRCPAINT);
              end;
            end;
          end;
      end;
    end;
  end;

begin
  if (Loc < 0) or (Loc >= G.lx * G.ly) then
    Tile := PoleTile(Loc)
  else
    Tile := MyMap[Loc];
  if ShowObjects and (Options and (1 shl moEditMode) = 0) and (Tile and fCity <> 0) then
    GetCityInfo(Loc, cix, CityInfo);
  if (y <= FTop - yyt * 2) or (y > FBottom) or (x <= FLeft - xxt * 2) or (x > FRight) then
  begin
    NameCity;
    ShowSpacePort;
    exit;
  end;
  if Tile and fTerrain = fUNKNOWN then
  begin
    NameCity;
    ShowSpacePort;
    exit;
  end;{square not discovered}

  if not (FoW and (Tile and fObserved = 0)) then
    PaintBorder;

  if (Loc >= 0) and (Loc < G.lx * G.ly) and (Loc = FAdviceLoc) then
    TSprite(x, y, 7 + 9 * 2);

  if (Loc >= 0) and (Loc < G.lx * G.ly) and (Tile and fSpecial <> 0) then {special ressources}
  begin
    dy := Loc div G.lx;
    if Tile and fTerrain < fForest then
      TSprite(x, y, Tile and fTerrain + (Tile and fSpecial shr 5) * 9)
    else if (Tile and fTerrain = fForest) and IsJungle(dy) then
      TSprite(x, y, 8 + 17 * 9 + (Tile and fSpecial shr 5) * 9)
    else
      TSprite(x, y, 8 + 2 * 9 + ((Tile and fTerrain - fForest) * 2 + Tile and fSpecial shr 5) * 9);
  end;

  if ShowObjects then
  begin
    if Tile and fTerImp = tiMine then
      TSprite(x, y, 2 + 9 * 12);
    if Tile and fTerImp = tiBase then
      TSprite(x, y, 4 + 9 * 12);
    if Tile and fPoll <> 0 then
      TSprite(x, y, 6 + 9 * 12);
    if Tile and fTerImp = tiFort then
    begin
      TSprite(x, y, 7 + 9 * 12);
      if Tile and fObserved = 0 then
        TSprite(x, y, 3 + 9 * 12);
    end;
  end;
  if Tile and fDeadLands <> 0 then
    TSprite(x, y, (12 + Tile shr 25 and 3) * 9 + 8);

  if Options and (1 shl moEditMode) <> 0 then
    fog := (Loc < 0) or (Loc >= G.lx * G.ly)
  //else if CityLoc>=0 then
  //  fog:= (Loc<0) or (Loc>=G.lx*G.ly) or (Distance(Loc,CityLoc)>5)
  else if ShowGrWall then
    fog := Tile and fGrWall = 0
  else
    fog := FoW and (Tile and fObserved = 0);
  if fog and ShowObjects then
    if Loc < -G.lx then
      BitBltTransparent(FOutput.Canvas, x, y + yyt, xxt * 2, yyt, 1 + 6 * (xxt * 2 + 1), 1 + yyt * 2 + 15 * (yyt * 3 + 1), terrainCurrent)
    else if Loc >= G.lx * (G.ly + 1) then
      BitBltTransparent(FOutput.Canvas, x, y, xxt * 2, yyt, 1 + 6 * (xxt * 2 + 1), 1 + yyt + 15 * (yyt * 3 + 1), terrainCurrent)
    else
      TSprite(x, y, 6 + 9 * 15, xxt <> 33);

  if FoW and (Tile and fObserved = 0) then
    PaintBorder;

{$IFNDEF SCR}
  // paint goto destination mark
  if DestinationMarkON and (CityOwner < 0) and (UnFocus >= 0) and
    (MyUn[UnFocus].Status and usGoto <> 0) then
  begin
    Destination := MyUn[UnFocus].Status shr 16;
    if (Destination = Loc) and (Destination <> MyUn[UnFocus].Loc) then
      if not UseBlink or BlinkOn then
        TSprite(x, y, 8 + 9 * 1)
      else
        TSprite(x, y, 8 + 9 * 2);
  end;
{$ENDIF}

  if Options and (1 shl moEditMode) <> 0 then
  begin
    if Tile and fPrefStartPos <> 0 then
      TSprite(x, y, 0 + 9 * 1)
    else if Tile and fStartPos <> 0 then
      TSprite(x, y, 0 + 9 * 2);
  end
  else if ShowObjects then
  begin
{  if (CityLoc<0) and (UnFocus>=0) and (Loc=MyUn[UnFocus].Loc) then
    if BlinkOn then TSprite(x,y,8+9*0)
    else TSprite(x,y,8+9*1);}

    NameCity;
    ShowSpacePort;
    if Tile and fCity <> 0 then
      PaintCity(x + xxt, y + yyt, CityInfo, CityOwner < 0);

    if (Tile and fUnit <> 0) and (Loc <> AttLoc) and ((Loc <> DefLoc) or (DefHealth <> 0))
    {$IFNDEF SCR}
      and ((CityOwner >= 0) or (UnFocus < 0) or not UseBlink or BlinkON or
      (Loc <> MyUn[UnFocus].Loc))
{$ENDIF}
      and ((Tile and fCity <> fCity) or (Loc = DefLoc)
      {$IFNDEF SCR}
      or (not UseBlink or BlinkON) and (UnFocus >= 0) and (Loc = MyUn[UnFocus].Loc)
{$ENDIF}
      ) then
    begin {unit}
      GetUnitInfo(Loc, uix, UnitInfo);
      if (Loc = DefLoc) and (DefHealth >= 0) then
        UnitInfo.Health := DefHealth;
      if (UnitInfo.Owner <> CityOwner) and not ((CityOwner = me) and
        (MyRO.Treaty[UnitInfo.Owner] = trAlliance)) then
      {$IFNDEF SCR}
        if (UnFocus >= 0) and (Loc = MyUn[UnFocus].Loc) then {active unit}
        begin
          Multi := UnitInfo.Flags and unMulti;
          MakeUnitInfo(me, MyUn[UnFocus], UnitInfo);
          UnitInfo.Flags := UnitInfo.Flags or Multi;
          PaintUnit(x + (xxt - xxu), y + (yyt - yyu_anchor), UnitInfo, MyUn[UnFocus].Status);
        end
        else if UnitInfo.Owner = me then
        begin
          if ClientMode = cMovieTurn then
            PaintUnit(x + (xxt - xxu), y + (yyt - yyu_anchor), UnitInfo, 0)
          // status is not set with precise timing during loading
          else
            PaintUnit(x + (xxt - xxu), y + (yyt - yyu_anchor), UnitInfo, MyUn[uix].Status);
          //      if Showuix then Textout(x+16,y+5,$80FF00,IntToStr(uix));
        end
        else
{$ENDIF}
          PaintUnit(x + (xxt - xxu), y + (yyt - yyu_anchor), UnitInfo, 0);
    end
    else if Tile and fHiddenUnit <> 0 then
      BitBltTransparent(FOutput.Canvas, x + (xxt - xxu), y + (yyt - yyu_anchor), xxu * 2, yyu * 2,
        1 + 5 * (xxu * 2 + 1), 1, stdUnitsPng)
    else if Tile and fStealthUnit <> 0 then
      BitBltTransparent(FOutput.Canvas, x + (xxt - xxu), y + (yyt - yyu_anchor), xxu * 2, yyu * 2,
        1 + 5 * (xxu * 2 + 1), 1 + 1 * (yyu * 2 + 1), stdUnitsPng);
  end;

  if ShowObjects and (Tile and fTerImp = tiFort) and (Tile and fObserved <> 0) then
    TSprite(x, y, 3 + 9 * 12);

  if (Loc >= 0) and (Loc < G.lx * G.ly) then
    if ShowLoc then
      Textout(x + xxt - 16, y + yyt - 9, $FFFF00, IntToStr(Loc))
    else if ShowDebug and (DebugMap <> nil) and (Loc >= 0) and (Loc < G.lx * G.ly) and
      (DebugMap[Loc] <> 0) then
      Textout(x + xxt - 16, y + yyt - 9, $00E0FF, IntToStr(integer(DebugMap[Loc])));
end;{PaintTileObjects}

procedure TIsoMap.PaintGrid(x, y, nx, ny: integer);

  procedure ClippedLine(dx0, dy0: integer; mirror: boolean);
  var
    x0, x1, dxmin, dymin, dxmax, dymax, n: integer;
  begin
    with FOutput.Canvas do
    begin
      dxmin := (FLeft - x) div xxt;
      dymin := (RealTop - y) div yyt;
      dxmax := (FRight - x - 1) div xxt + 1;
      dymax := (RealBottom - y - 1) div yyt + 1;
      n := dymax - dy0;
      if mirror then
      begin
        if dx0 - dxmin < n then
          n := dx0 - dxmin;
        if dx0 > dxmax then
        begin
          n := n - (dx0 - dxmax);
          dy0 := dy0 + (dx0 - dxmax);
          dx0 := dxmax;
        end;
        if dy0 < dymin then
        begin
          n := n - (dymin - dy0);
          dx0 := dx0 - (dymin - dy0);
          dy0 := dymin;
        end;
      end
      else
      begin
        if dxmax - dx0 < n then
          n := dxmax - dx0;
        if dx0 < dxmin then
        begin
          n := n - (dxmin - dx0);
          dy0 := dy0 + (dxmin - dx0);
          dx0 := dxmin;
        end;
        if dy0 < dymin then
        begin
          n := n - (dymin - dy0);
          dx0 := dx0 + (dymin - dy0);
          dy0 := dymin;
        end;
      end;
      if n <= 0 then
        exit;
      if mirror then
      begin
        x0 := x + dx0 * xxt - 1;
        x1 := x + (dx0 - n) * xxt - 1;
      end
      else
      begin
        x0 := x + dx0 * xxt;
        x1 := x + (dx0 + n) * xxt;
      end;
      moveto(x0, y + dy0 * yyt);
      lineto(x1, y + (dy0 + n) * yyt);
    end;
  end;

var
  i: integer;
begin
  FOutput.Canvas.pen.color := $000000; //$FF shl (8*random(3));
  for i := 0 to nx div 2 do
    ClippedLine(i * 2, 0, False);
  for i := 1 to (nx + 1) div 2 do
    ClippedLine(i * 2, 0, True);
  for i := 0 to ny div 2 do
  begin
    ClippedLine(0, 2 * i + 2, False);
    ClippedLine(nx + 1, 2 * i + 1 + nx and 1, True);
  end;
end;

procedure TIsoMap.Paint(x, y, Loc, nx, ny, CityLoc, CityOwner: integer;
  UseBlink: boolean; CityAllowClick: boolean);

  function IsShoreTile(Loc: integer): boolean;
  const
    Dirx: array[0..7] of integer = (1, 2, 1, 0, -1, -2, -1, 0);
    Diry: array[0..7] of integer = (-1, 0, 1, 2, 1, 0, -1, -2);
  var
    Dir, ConnLoc: integer;
  begin
    Result := False;
    for Dir := 0 to 7 do
    begin
      ConnLoc := dLoc(Loc, Dirx[Dir], Diry[Dir]);
      if (ConnLoc < 0) or (ConnLoc >= G.lx * G.ly) or
        ((MyMap[ConnLoc] - 2) and fTerrain < 13) then
        Result := True;
    end;
  end;

  procedure ShadeOutside(x0, y0, x1, y1, xm, ym: integer);
  const
    rShade = 3.75;

    procedure MakeDark(line: pointer; length: integer);
    type
      TCardArray = array[0..9999] of cardinal;
      PCardArray = ^TCardArray;
      TByteArray = array[0..9999] of byte;
      PByteArray = ^TByteArray;
    var
      i, rest: integer;
    begin
      for i := length * 3 div 4 - 1 downto 0 do
        PCardArray(line)[i] := PCardArray(line)[i] shr 1 and $7F7F7F7F;
      rest := (length * 3 div 4) * 4;
      for i := length * 3 mod 4 - 1 downto 0 do
        PByteArray(line)[rest + i] := PByteArray(line)[rest + i] shr 1 and $7F;
    end;

  type
    TLine = array[0..99999, 0..2] of byte;
  var
    y, wBright: integer;
    y_n, w_n: single;
    line: ^TLine;
  begin
    for y := y0 to y1 - 1 do
    begin
      FOutput.BeginUpdate();
      line := FOutput.ScanLine[y];
      FOutput.EndUpdate();
      y_n := (y - ym) / yyt;
      if abs(y_n) < rShade then
      begin
        w_n := sqrt(sqr(rShade) - sqr(y_n));
        wBright := trunc(w_n * xxt + 0.5);
        MakeDark(@line[x0], xm - x0 - wBright);
        MakeDark(@line[xm + wBright], x1 - xm - wBright);
      end
      else
        MakeDark(@line[x0], x1 - x0);
    end;
  end;

  procedure CityGrid(xm, ym: integer);
  var
    i: integer;
  begin
    with FOutput.Canvas do
    begin
      if CityAllowClick then
        pen.Color := $FFFFFF
      else
        pen.color := $000000;
      pen.Width := 1;
      for i := 0 to 3 do
      begin
        moveto(xm - xxt * (4 - i), ym + yyt * (1 + i));
        lineto(xm + xxt * (1 + i), ym - yyt * (4 - i));
        moveto(xm - xxt * (4 - i), ym - yyt * (1 + i));
        lineto(xm + xxt * (1 + i), ym + yyt * (4 - i));
      end;
      moveto(xm - xxt * 4, ym + yyt * 1);
      lineto(xm - xxt * 1, ym + yyt * 4);
      moveto(xm + xxt * 1, ym + yyt * 4);
      lineto(xm + xxt * 4, ym + yyt * 1);
      moveto(xm - xxt * 4, ym - yyt * 1);
      lineto(xm - xxt * 1, ym - yyt * 4);
      moveto(xm + xxt * 1, ym - yyt * 4);
      lineto(xm + xxt * 4, ym - yyt * 1);
      pen.Width := 1;
    end;
  end;

var
  dx, dy, xm, ym, ALoc, BLoc, ATer, BTer, Aix, bix: integer;
begin
  FoW := True;
  ShowLoc := Options and (1 shl moLocCodes) <> 0;
  ShowDebug := pDebugMap >= 0;
  ShowObjects := (CityOwner >= 0) or (Options and (1 shl moBareTerrain) = 0);
  ShowCityNames := ShowObjects and (CityOwner < 0) and (Options and (1 shl moCityNames) <> 0);
  ShowBorder := True;
  ShowMyBorder := CityOwner < 0;
  ShowGrWall := (CityOwner < 0) and (Options and (1 shl moGreatWall) <> 0);
  if ShowDebug then
    Server(sGetDebugMap, me, pDebugMap, DebugMap)
  else
    DebugMap := nil;
  with FOutput.Canvas do
  begin
    RealTop := y - ((Loc + 12345 * G.lx) div G.lx - 12345) * yyt;
    RealBottom := y + (G.ly - ((Loc + 12345 * G.lx) div G.lx - 12345) + 3) * yyt;
    Brush.Color := EmptySpaceColor;
    if RealTop > FTop then
      FillRect(Rect(FLeft, FTop, FRight, RealTop))
    else
      RealTop := FTop;
    if RealBottom < FBottom then
      FillRect(Rect(FLeft, RealBottom, FRight, FBottom))
    else
      RealBottom := FBottom;
    Brush.Color := $000000;
    FillRect(Rect(FLeft, RealTop, FRight, RealBottom));
    Brush.Style := bsClear;
  end;

  for dy := 0 to ny + 1 do
    if (Loc + dy * G.lx >= 0) and (Loc + (dy - 3) * G.lx < G.lx * G.ly) then
      for dx := 0 to nx do
      begin
        ALoc := dLoc(Loc, dx - (dy + dx) and 1, dy - 2);
        BLoc := dLoc(Loc, dx - (dy + dx + 1) and 1, dy - 1);
        if (ALoc < 0) or (ALoc >= G.lx * G.ly) then
          ATer := PoleTile(ALoc) and fTerrain
        else
          ATer := MyMap[ALoc] and fTerrain;
        if (BLoc < 0) or (BLoc >= G.lx * G.ly) then
          BTer := PoleTile(BLoc) and fTerrain
        else
          BTer := MyMap[BLoc] and fTerrain;

        if (ATer <> fUNKNOWN) or (BTer <> fUNKNOWN) then
          if ((ATer < fGrass) or (ATer = fUNKNOWN)) and ((BTer < fGrass) or (BTer = fUNKNOWN)) then
          begin
            if ATer = fUNKNOWN then
              Aix := 0
            else if IsShoreTile(ALoc) then
              if ATer = fOcean then
                Aix := -1
              else
                Aix := 1
            else
              Aix := ATer + 2;
            if BTer = fUNKNOWN then
              bix := 0
            else if IsShoreTile(BLoc) then
              if BTer = fOcean then
                bix := -1
              else
                bix := 1
            else
              bix := BTer + 2;
            if (Aix > 1) or (bix > 1) then
            begin
              if aix = -1 then
                if bix = fOcean + 2 then
                begin
                  aix := 0;
                  bix := 0;
                end
                else
                begin
                  aix := 0;
                  bix := 1;
                end
              else if bix = -1 then
                if aix = fOcean + 2 then
                begin
                  aix := 1;
                  bix := 1;
                end
                else
                begin
                  aix := 1;
                  bix := 0;
                end;
              BitBlt_to_isoengine(OceanPatch, x + dx * xxt, y + dy * yyt, xxt, yyt,
                Aix * (xxt * 2) + (dx + dy + 1) and 1 * xxt, bix * yyt, SRCCOPY);
            end;
          end
          else
          begin
            if ATer = fUNKNOWN then
              Aix := 0
            else if (ALoc >= 0) and (ALoc < G.lx * G.ly) and (MyMap[ALoc] and fDeadLands <> 0) then
              Aix := -2
            else if ATer = fOcean then
              Aix := -1
            else if ATer = fShore then
              Aix := 1
            else if ATer >= fForest then
              Aix := 8
            else
              Aix := ATer;
            if BTer = fUNKNOWN then
              bix := 0
            else if (BLoc >= 0) and (BLoc < G.lx * G.ly) and (MyMap[BLoc] and fDeadLands <> 0) then
              Bix := -2
            else if BTer = fOcean then
              bix := -1
            else if BTer = fShore then
              bix := 1
            else if BTer >= fForest then
              bix := 8
            else
              bix := BTer;
            if (Aix = -2) and (Bix = -2) then
            begin
              Aix := fDesert;
              Bix := fDesert;
            end
            else if Aix = -2 then
              if Bix < 2 then
                Aix := 8
              else
                Aix := Bix
            else if Bix = -2 then
              if Aix < 2 then
                Bix := 8
              else
                Bix := Aix;
            if Aix = -1 then
              BitBlt_to_isoengine(terrainCurrent, x + dx * xxt, y + dy * yyt, xxt, yyt,
                1 + 6 * (xxt * 2 + 1) + (dx + dy + 1) and 1 * xxt, 1 + yyt, SRCCOPY) // arctic <-> ocean
            else if bix = -1 then
              BitBlt_to_isoengine(terrainCurrent, x + dx * xxt, y + dy * yyt, xxt,
                yyt, 1 + 6 * (xxt * 2 + 1) + xxt - (dx + dy + 1) and 1 * xxt, 1 + yyt * 2, SRCCOPY) // arctic <-> ocean
            else
              BitBlt_to_isoengine(LandPatch, x + dx * xxt, y + dy * yyt, xxt, yyt,
                Aix * (xxt * 2) + (dx + dy + 1) and 1 * xxt, bix * yyt, SRCCOPY);
          end;
      end;

  for dy := -2 to ny + 1 do
    for dx := -1 to nx do
      if (dx + dy) and 1 = 0 then
        PaintShore(x + xxt * dx, y + yyt + yyt * dy, dLoc(Loc, dx, dy));
  for dy := -2 to ny + 1 do
    for dx := -1 to nx do
      if (dx + dy) and 1 = 0 then
        PaintTileExtraTerrain(x + xxt * dx, y + yyt + yyt * dy, dLoc(Loc, dx, dy));
  if CityOwner >= 0 then
  begin
    for dy := -2 to ny + 1 do
      for dx := -2 to nx + 1 do
        if (dx + dy) and 1 = 0 then
        begin
          ALoc := dLoc(Loc, dx, dy);
          if Distance(ALoc, CityLoc) > 5 then
            PaintTileObjects(x + xxt * dx, y + yyt + yyt * dy, ALoc, CityLoc, CityOwner, UseBlink);
        end;
    dx := ((CityLoc mod G.lx * 2 + CityLoc div G.lx and 1) -
      ((Loc + 666 * G.lx) mod G.lx * 2 + (Loc + 666 * G.lx) div G.lx and 1) + 3 * G.lx) mod
      (2 * G.lx) - G.lx;
    dy := CityLoc div G.lx - (Loc + 666 * G.lx) div G.lx + 666;
    xm := x + (dx + 1) * xxt;
    ym := y + (dy + 1) * yyt + yyt;
    ShadeOutside(FLeft, FTop, FRight, FBottom, xm, ym);
    CityGrid(xm, ym);
    for dy := -2 to ny + 1 do
      for dx := -2 to nx + 1 do
        if (dx + dy) and 1 = 0 then
        begin
          ALoc := dLoc(Loc, dx, dy);
          if Distance(ALoc, CityLoc) <= 5 then
            PaintTileObjects(x + xxt * dx, y + yyt + yyt * dy, ALoc, CityLoc, CityOwner, UseBlink);
        end;
  end
  else
  begin
    if ShowLoc or (Options and (1 shl moEditMode) <> 0) or
      (Options and (1 shl moGrid) <> 0) then
      PaintGrid(x, y, nx, ny);
    for dy := -2 to ny + 1 do
      for dx := -2 to nx + 1 do
        if (dx + dy) and 1 = 0 then
          PaintTileObjects(x + xxt * dx, y + yyt + yyt * dy, dLoc(Loc, dx, dy), CityLoc, CityOwner, UseBlink);
  end;

  //frame(FOutput.Canvas,x+1,y+1,x+nx*33+33-2,y+ny*16+32-2,$FFFF,$FFFF);
end; {Paint}

procedure TIsoMap.AttackBegin(const ShowMove: TShowMove);
begin
  AttLoc := ShowMove.FromLoc;
  DefLoc := dLoc(AttLoc, ShowMove.dx, ShowMove.dy);
  DefHealth := -1;
end;

procedure TIsoMap.AttackEffect(const ShowMove: TShowMove);
begin
  DefHealth := ShowMove.EndHealthDef;
end;

procedure TIsoMap.AttackEnd;
begin
  AttLoc := -1;
  DefLoc := -1;
end;


initialization

  NoMap := nil;
  LandPatch := nil;
  OceanPatch := nil;
  Borders := nil;
end.
