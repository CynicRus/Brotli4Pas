unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, 
  System.IOUtils, System.Diagnostics, Vcl.Graphics, Vcl.Controls, Vcl.Forms, 
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, brotli, libbrotli;

type
  TForm1 = class(TForm)
    pnlTop: TPanel;
    lblTitle: TLabel;
    pnlMain: TPanel;
    grpCompress: TGroupBox;
    btnSelectFile: TButton;
    edtInputFile: TEdit;
    lblInputSize: TLabel;
    btnCompress: TButton;
    grpDecompress: TGroupBox;
    btnSelectCompressed: TButton;
    edtCompressedFile: TEdit;
    lblCompressedSize: TLabel;
    btnDecompress: TButton;
    pnlLog: TPanel;
    lblLog: TLabel;
    memoLog: TMemo;
    openFileDialog: TOpenDialog;
    saveFileDialog: TSaveDialog;
    pbProgress: TProgressBar;
    lblStatus: TLabel;
    cmbQuality: TComboBox;
    lblQuality: TLabel;
    lblWindow: TLabel;
    cmbWindow: TComboBox;
    chkVerify: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnSelectFileClick(Sender: TObject);
    procedure btnSelectCompressedClick(Sender: TObject);
    procedure btnCompressClick(Sender: TObject);
    procedure btnDecompressClick(Sender: TObject);
  private
    FInputFile: string;
    FCompressedFile: string;
    procedure Log(const Msg: string);
    procedure CompressFile;
    procedure DecompressFile;
    function FormatFileSize(Size: Int64): string;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Настройка качества сжатия (0-11)
  cmbQuality.Items.Clear;
  cmbQuality.Items.Add('0 - Fastest');
  cmbQuality.Items.Add('1');
  cmbQuality.Items.Add('4 - Fast');
  cmbQuality.Items.Add('6 - Default');
  cmbQuality.Items.Add('9 - Better');
  cmbQuality.Items.Add('11 - Best');
  cmbQuality.ItemIndex := 3; // Default = 6
  
  // Настройка размера окна (10-24)
  cmbWindow.Items.Clear;
  cmbWindow.Items.Add('16 - 64KB');
  cmbWindow.Items.Add('18 - 256KB');
  cmbWindow.Items.Add('20 - 1MB');
  cmbWindow.Items.Add('22 - 4MB (Default)');
  cmbWindow.Items.Add('24 - 16MB (Large)');
  cmbWindow.ItemIndex := 3; // Default = 22
  
  pbProgress.Position := 0;
  lblStatus.Caption := 'Ready';
  memoLog.Clear;
  Log('Google Brotli Compression Demo initialized');
  Log('Version: ' + BrotliEncoderVersionString);
end;

procedure TForm1.Log(const Msg: string);
begin
  memoLog.Lines.Add(Format('[%s] %s', [FormatDateTime('hh:nn:ss', Now), Msg]));
  Application.ProcessMessages;
end;

function TForm1.FormatFileSize(Size: Int64): string;
begin
  if Size < 1024 then
    Result := Format('%d B', [Size])
  else if Size < 1024 * 1024 then
    Result := Format('%.2f KB', [Size / 1024])
  else if Size < 1024 * 1024 * 1024 then
    Result := Format('%.2f MB', [Size / 1024 / 1024])
  else
    Result := Format('%.2f GB', [Size / 1024 / 1024 / 1024]);
end;

procedure TForm1.btnSelectFileClick(Sender: TObject);
begin
  openFileDialog.Filter := 'All Files (*.*)|*.*';
  openFileDialog.Title := 'Select File to Compress';
  
  if openFileDialog.Execute then
  begin
    FInputFile := openFileDialog.FileName;
    edtInputFile.Text := ExtractFileName(FInputFile);
    edtInputFile.Hint := FInputFile;
    
    if FileExists(FInputFile) then
    begin
      lblInputSize.Caption := 'Size: ' + FormatFileSize(TFile.GetSize(FInputFile));
      Log('Selected file: ' + FInputFile);
      btnCompress.Enabled := True;
    end;
  end;
end;

procedure TForm1.btnSelectCompressedClick(Sender: TObject);
begin
  openFileDialog.Filter := 'Brotli Files (*.br)|*.br|All Files (*.*)|*.*';
  openFileDialog.Title := 'Select Brotli File to Decompress';
  
  if openFileDialog.Execute then
  begin
    FCompressedFile := openFileDialog.FileName;
    edtCompressedFile.Text := ExtractFileName(FCompressedFile);
    edtCompressedFile.Hint := FCompressedFile;
    
    if FileExists(FCompressedFile) then
    begin
      lblCompressedSize.Caption := 'Size: ' + FormatFileSize(TFile.GetSize(FCompressedFile));
      Log('Selected compressed file: ' + FCompressedFile);
      btnDecompress.Enabled := True;
    end;
  end;
end;

procedure TForm1.btnCompressClick(Sender: TObject);
begin
  if FInputFile.IsEmpty or not FileExists(FInputFile) then
  begin
    ShowMessage('Please select an input file first!');
    Exit;
  end;
  
  saveFileDialog.Filter := 'Brotli Files (*.br)|*.br';
  saveFileDialog.FileName := TPath.GetFileNameWithoutExtension(FInputFile) + '.br';
  saveFileDialog.Title := 'Save Compressed File';
  
  if saveFileDialog.Execute then
  begin
    FCompressedFile := saveFileDialog.FileName;
    CompressFile;
  end;
end;

procedure TForm1.btnDecompressClick(Sender: TObject);
var
  OutputFile: string;
begin
  if FCompressedFile.IsEmpty or not FileExists(FCompressedFile) then
  begin
    ShowMessage('Please select a compressed file first!');
    Exit;
  end;
  
  saveFileDialog.Filter := 'All Files (*.*)|*.*';
  saveFileDialog.FileName := TPath.GetFileNameWithoutExtension(FCompressedFile);
  saveFileDialog.Title := 'Save Decompressed File';
  
  if saveFileDialog.Execute then
  begin
    OutputFile := saveFileDialog.FileName;
    DecompressFile;
  end;
end;

procedure TForm1.CompressFile;
const
  BUFFER_SIZE = 64 * 1024; // 64KB буфер
  QualityMap: array[0..5] of Integer = (0, 1, 4, 6, 9, 11);
  WindowMap: array[0..4] of Integer = (16, 18, 20, 22, 24);
var
  InputStream: TFileStream;
  OutputFile: string;
  BrotliStream: TBrotliCompressionStream;
  Buffer: array[0..BUFFER_SIZE-1] of Byte;
  BytesRead: Integer;
  TotalRead: Int64;
  Quality, LgWin: uint32_t;
  SW: TStopwatch;
  OriginalSize, CompressedSize: Int64;
  Ratio: Double;
begin
  OutputFile := FCompressedFile;
  Quality := QualityMap[cmbQuality.ItemIndex];
  LgWin := WindowMap[cmbWindow.ItemIndex];
  
  btnCompress.Enabled := False;
  pbProgress.Position := 0;
  lblStatus.Caption := 'Compressing...';
  Log('─────────────────────────────────');
  Log('Starting compression...');
  Log('Quality: ' + IntToStr(Quality) + ', Window: ' + IntToStr(LgWin));
  
  try
    InputStream := TFileStream.Create(FInputFile, fmOpenRead or fmShareDenyWrite);
    try
      OriginalSize := InputStream.Size;
      BrotliStream := TBrotliCompressionStream.Create(
        TFileStream.Create(OutputFile, fmCreate),
        Quality,
        LgWin,
        BROTLI_MODE_GENERIC,
        []
      );
      try
        SW := TStopwatch.StartNew;
        TotalRead := 0;
        
        repeat
          BytesRead := InputStream.Read(Buffer, BUFFER_SIZE);
          if BytesRead > 0 then
          begin
            BrotliStream.Write(Buffer, BytesRead);
            Inc(TotalRead, BytesRead);
            pbProgress.Position := Round((TotalRead / OriginalSize) * 100);
            Application.ProcessMessages;
          end;
        until BytesRead = 0;
        
        BrotliStream.Finish; // Важно! Завершить сжатие
        SW.Stop;
        
        CompressedSize := TFile.GetSize(OutputFile);
        Ratio := (1 - (CompressedSize / OriginalSize)) * 100;
        
        Log('✓ Compression successful!');
        Log('Original size: ' + FormatFileSize(OriginalSize));
        Log('Compressed size: ' + FormatFileSize(CompressedSize));
        Log(Format('Compression ratio: %.2f%%', [Ratio]));
        Log(Format('Time: %.2f ms', [SW.Elapsed.TotalMilliseconds]));
        Log(Format('Speed: %.2f MB/s', [OriginalSize / 1024 / 1024 / SW.Elapsed.TotalSeconds]));
        
        pbProgress.Position := 100;
        lblStatus.Caption := 'Compression complete!';
        
        // Автозагрузка результата в decompress секцию
        edtCompressedFile.Text := ExtractFileName(OutputFile);
        edtCompressedFile.Hint := OutputFile;
        lblCompressedSize.Caption := 'Size: ' + FormatFileSize(CompressedSize);
        btnDecompress.Enabled := True;
        
        if chkVerify.Checked then
        begin
          Log('Verifying compressed data...');
          // TODO: добавить верификацию через декомпрессию во временный файл
        end;
        
      finally
        BrotliStream.Free;
      end;
    finally
      InputStream.Free;
    end;
  except
    on E: Exception do
    begin
      Log('✗ ERROR: ' + E.Message);
      lblStatus.Caption := 'Compression failed!';
      ShowMessage('Compression error: ' + E.Message);
    end;
  end;
  
  btnCompress.Enabled := True;
  pbProgress.Position := 0;
end;

procedure TForm1.DecompressFile;
const
  BUFFER_SIZE = 64 * 1024;
var
  InputStream: TFileStream;
  OutputFile: string;
  BrotliStream: TBrotliDecompressionStream;
  Buffer: array[0..BUFFER_SIZE-1] of Byte;
  BytesRead: Integer;
  TotalWritten: Int64;
  SW: TStopwatch;
  CompressedSize, DecompressedSize: Int64;
  OutStream: TFileStream;
begin
  OutputFile := saveFileDialog.FileName;
  
  btnDecompress.Enabled := False;
  pbProgress.Position := 0;
  lblStatus.Caption := 'Decompressing...';
  Log('─────────────────────────────────');
  Log('Starting decompression...');
  
  try
    InputStream := TFileStream.Create(FCompressedFile, fmOpenRead or fmShareDenyWrite);
    try
      CompressedSize := InputStream.Size;
      BrotliStream := TBrotliDecompressionStream.Create(InputStream, [brLeaveOpen]);
      try
        OutStream := TFileStream.Create(OutputFile, fmCreate);
        try
          SW := TStopwatch.StartNew;
          TotalWritten := 0;
          
          repeat
            BytesRead := BrotliStream.Read(Buffer, BUFFER_SIZE);
            if BytesRead > 0 then
            begin
              OutStream.Write(Buffer, BytesRead);
              Inc(TotalWritten, BytesRead);
              // Для декомпрессии сложно показать точный прогресс, используем анимацию
              pbProgress.Position := (pbProgress.Position + 5) mod 100;
              Application.ProcessMessages;
            end;
          until BytesRead = 0;
          
          SW.Stop;
          DecompressedSize := TotalWritten;
          
          Log('✓ Decompression successful!');
          Log('Compressed size: ' + FormatFileSize(CompressedSize));
          Log('Decompressed size: ' + FormatFileSize(DecompressedSize));
          Log(Format('Time: %.2f ms', [SW.Elapsed.TotalMilliseconds]));
          Log(Format('Speed: %.2f MB/s', [DecompressedSize / 1024 / 1024 / SW.Elapsed.TotalSeconds]));
          
          pbProgress.Position := 100;
          lblStatus.Caption := 'Decompression complete!';
          
        finally
          OutStream.Free;
        end;
      finally
        BrotliStream.Free;
      end;
    finally
      InputStream.Free;
    end;
  except
    on E: Exception do
    begin
      Log('✗ ERROR: ' + E.Message);
      lblStatus.Caption := 'Decompression failed!';
      ShowMessage('Decompression error: ' + E.Message);
    end;
  end;
  
  btnDecompress.Enabled := True;
  pbProgress.Position := 0;
end;

end.