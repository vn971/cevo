object RatesDlg: TRatesDlg
  Left = 262
  Top = 517
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 223
  ClientWidth = 312
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDefaultPosOnly
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object CloseBtn: TButtonB
    Left = 274
    Top = 6
    Width = 25
    Height = 25
    Down = False
    Permanent = False
    OnClick = CloseBtnClick
    ButtonIndex = 0
  end
  object LuxBtn: TButtonC
    Left = 156
    Top = 64
    Width = 12
    Height = 12
    Down = False
    Permanent = False
    OnClick = TaxLuxBtnClick
    ButtonIndex = 5
  end
  object ScienceBtn: TButtonC
    Left = 144
    Top = 64
    Width = 12
    Height = 12
    Down = False
    Permanent = False
    OnClick = TaxLuxBtnClick
    ButtonIndex = 4
  end
  object TaxUpBtn: TButtonC
    Left = 205
    Top = 170
    Width = 12
    Height = 12
    Down = False
    Permanent = False
    OnClick = TaxLuxBtnClick
    ButtonIndex = 1
  end
  object TaxDownBtn: TButtonC
    Left = 205
    Top = 182
    Width = 12
    Height = 12
    Down = False
    Permanent = False
    OnClick = TaxLuxBtnClick
    ButtonIndex = 0
  end
end
