object StartDlg: TStartDlg
  Left = 246
  Top = 109
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'C-evo'
  ClientHeight = 326
  ClientWidth = 556
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDefaultPosOnly
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHide = FormHide
  OnKeyDown = FormKeyDown
  OnMouseDown = FormMouseDown
  OnMouseMove = FormMouseMove
  OnMouseUp = FormMouseUp
  OnPaint = FormPaint
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object StartBtn: TButtonA
    Tag = 15104
    Left = 389
    Top = 286
    Width = 100
    Height = 25
    Down = False
    Permanent = False
    OnClick = StartBtnClick
  end
  object Down1Btn: TButtonC
    Tag = 4096
    Left = 522
    Top = 111
    Width = 12
    Height = 12
    Down = False
    Permanent = False
    OnClick = Down1BtnClick
    ButtonIndex = 0
  end
  object Up1Btn: TButtonC
    Tag = 4096
    Left = 522
    Top = 99
    Width = 12
    Height = 12
    Down = False
    Permanent = False
    OnClick = Up1BtnClick
    ButtonIndex = 1
  end
  object RenameBtn: TButtonB
    Tag = 10240
    Left = 412
    Top = 98
    Width = 25
    Height = 25
    Visible = False
    Down = False
    Permanent = False
    OnClick = RenameBtnClick
    ButtonIndex = 31
  end
  object DeleteBtn: TButtonB
    Tag = 10240
    Left = 441
    Top = 98
    Width = 25
    Height = 25
    Visible = False
    Down = False
    Permanent = False
    OnClick = DeleteBtnClick
    ButtonIndex = 21
  end
  object Down2Btn: TButtonC
    Tag = 6912
    Left = 522
    Top = 249
    Width = 12
    Height = 12
    Visible = False
    Down = False
    Permanent = False
    OnClick = Down2BtnClick
    ButtonIndex = 0
  end
  object Up2Btn: TButtonC
    Tag = 6912
    Left = 522
    Top = 237
    Width = 12
    Height = 12
    Visible = False
    Down = False
    Permanent = False
    OnClick = Up2BtnClick
    ButtonIndex = 1
  end
  object QuitBtn: TButtonB
    Tag = 268435200
    Left = 530
    Top = 7
    Width = 25
    Height = 25
    Down = False
    Permanent = False
    OnClick = QuitBtnClick
    ButtonIndex = 0
  end
  object CustomizeBtn: TButtonC
    Tag = 768
    Left = 120
    Top = 302
    Width = 12
    Height = 12
    Down = False
    Permanent = False
    OnClick = CustomizeBtnClick
    ButtonIndex = 0
  end
  object AutoDiffUpBtn: TButtonC
    Left = 280
    Top = 237
    Width = 12
    Height = 12
    Down = False
    Permanent = False
    OnClick = AutoDiffUpBtnClick
    ButtonIndex = 1
  end
  object AutoDiffDownBtn: TButtonC
    Left = 280
    Top = 249
    Width = 12
    Height = 12
    Down = False
    Permanent = False
    OnClick = AutoDiffDownBtnClick
    ButtonIndex = 0
  end
  object AutoEnemyUpBtn: TButtonC
    Left = 206
    Top = 152
    Width = 12
    Height = 12
    Down = False
    Permanent = False
    OnClick = AutoEnemyUpBtnClick
    ButtonIndex = 1
  end
  object AutoEnemyDownBtn: TButtonC
    Left = 206
    Top = 164
    Width = 12
    Height = 12
    Down = False
    Permanent = False
    OnClick = AutoEnemyDownBtnClick
    ButtonIndex = 0
  end
  object ReplayBtn: TButtonB
    Tag = 2048
    Left = 352
    Top = 286
    Width = 25
    Height = 25
    Down = False
    Permanent = False
    OnClick = ReplayBtnClick
    ButtonIndex = 19
  end
  object List: TListBox
    Tag = 15360
    Left = 45
    Top = 64
    Width = 266
    Height = 238
    TabStop = False
    BorderStyle = bsNone
    Color = clBlack
    ExtendedSelect = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4176863
    Font.Height = -15
    Font.Name = 'Times New Roman'
    Font.Style = [fsBold]
    IntegralHeight = True
    ItemHeight = 17
    ParentFont = False
    TabOrder = 0
    Visible = False
    OnClick = ListClick
  end
  object PopupMenu1: TPopupMenu
    Left = 8
    Top = 8
  end
end
