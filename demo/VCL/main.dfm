object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Google Brotli Compression Tool'
  ClientHeight = 650
  ClientWidth = 850
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 15
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 850
    Height = 65
    Align = alTop
    BevelOuter = bvNone
    Color = 2236962
    ParentBackground = False
    TabOrder = 0
    object lblTitle: TLabel
      Left = 16
      Top = 16
      Width = 278
      Height = 32
      Caption = 'Brotli Compression Demo'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -24
      Font.Name = 'Segoe UI Semibold'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 65
    Width = 850
    Height = 585
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object grpCompress: TGroupBox
      Left = 16
      Top = 16
      Width = 400
      Height = 250
      Caption = '  Compress File  '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      object lblInputSize: TLabel
        Left = 16
        Top = 80
        Width = 63
        Height = 15
        Caption = 'Size: 0 bytes'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblQuality: TLabel
        Left = 16
        Top = 112
        Width = 41
        Height = 15
        Caption = 'Quality:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object lblWindow: TLabel
        Left = 16
        Top = 160
        Width = 70
        Height = 15
        Caption = 'Window Size:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object btnSelectFile: TButton
        Left = 16
        Top = 32
        Width = 120
        Height = 32
        Caption = 'Select File...'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = btnSelectFileClick
      end
      object edtInputFile: TEdit
        Left = 152
        Top = 32
        Width = 233
        Height = 23
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        TabOrder = 1
        TextHint = 'No file selected'
      end
      object cmbQuality: TComboBox
        Left = 16
        Top = 133
        Width = 369
        Height = 23
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
      end
      object cmbWindow: TComboBox
        Left = 16
        Top = 181
        Width = 369
        Height = 23
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
      end
      object btnCompress: TButton
        Left = 16
        Top = 210
        Width = 369
        Height = 32
        Caption = 'Compress'
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 4
        OnClick = btnCompressClick
      end
    end
    object grpDecompress: TGroupBox
      Left = 432
      Top = 16
      Width = 400
      Height = 250
      Caption = '  Decompress File  '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      object lblCompressedSize: TLabel
        Left = 16
        Top = 80
        Width = 63
        Height = 15
        Caption = 'Size: 0 bytes'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object btnSelectCompressed: TButton
        Left = 16
        Top = 32
        Width = 120
        Height = 32
        Caption = 'Select File...'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = btnSelectCompressedClick
      end
      object edtCompressedFile: TEdit
        Left = 152
        Top = 32
        Width = 233
        Height = 23
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        TabOrder = 1
        TextHint = 'No file selected'
      end
      object chkVerify: TCheckBox
        Left = 16
        Top = 112
        Width = 369
        Height = 17
        Caption = 'Verify integrity after compression'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
      end
      object btnDecompress: TButton
        Left = 16
        Top = 210
        Width = 369
        Height = 32
        Caption = 'Decompress'
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
        OnClick = btnDecompressClick
      end
    end
    object pnlLog: TPanel
      Left = 16
      Top = 280
      Width = 816
      Height = 289
      BevelOuter = bvNone
      TabOrder = 2
      object lblLog: TLabel
        Left = 0
        Top = 0
        Width = 816
        Height = 20
        Align = alTop
        Caption = '  Activity Log'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitWidth = 92
      end
      object lblStatus: TLabel
        Left = 0
        Top = 274
        Width = 816
        Height = 15
        Align = alBottom
        Alignment = taCenter
        Caption = 'Ready'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitWidth = 34
      end
      object memoLog: TMemo
        Left = 0
        Top = 20
        Width = 816
        Height = 234
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object pbProgress: TProgressBar
        Left = 0
        Top = 254
        Width = 816
        Height = 20
        Align = alBottom
        TabOrder = 1
      end
    end
  end
  object openFileDialog: TOpenDialog
    Left = 368
    Top = 80
  end
  object saveFileDialog: TSaveDialog
    Left = 456
    Top = 80
  end
end
