unit brotli;

{ ******************************************************************************* }
{ }
{ Brotli streams for Delphi/FPC (Win32/Win64/Linux64/macOS) }
{ - Compression stream: write -> brotli -> underlying stream }
{ - Decompression stream: read <- brotli <- underlying stream }
{ }
{ Notes: }
{ * Uses BrotliEncoderCompressStream / BrotliDecoderDecompressStream. }
{ * Underlying stream is NOT owned by default. }
{ }
{ ******************************************************************************* }
interface

uses
{$IFNDEF FPC}
{$IFNDEF DELPHI_7}
  System.Classes, System.SysUtils, zlib {$IFDEF MSWINDOWS},
  Winapi.Windows, System.Math, {$ENDIF}libbrotli;
{$ELSE}
Windows, Classes, SysUtils, libbrotli;
{$ENDIF}
{$ENDIF}
{$IFDEF FPC}
Classes, SysUtils, LCLIntf, libbrotli;
{$ENDIF}

const
  BROTLI_BUFFER_SIZE = 524288; // 1 << 19

type
  EBrotliError = class(Exception);
  EBrotliCompressionError = class(EBrotliError);
  EBrotliDecompressionError = class(EBrotliError);

  TBrotliStreamOption = (brLeaveOpen);
  TBrotliStreamOptions = set of TBrotliStreamOption;

type
  TCustomBrotliStream = class(TStream)
  private
    FStrm: TStream;
    FStrmPos: Int64;
    FOnProgress: TNotifyEvent;
    FOptions: TBrotliStreamOptions;
  protected
    procedure Progress(Sender: TObject); dynamic;
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
    constructor Create(Strm: TStream; Options: TBrotliStreamOptions);
    property BaseStream: TStream read FStrm;
    property BasePos: Int64 read FStrmPos;
    property Options: TBrotliStreamOptions read FOptions;
  end;

  { TBrotliCompressionStream }

  TBrotliCompressionStream = class(TCustomBrotliStream)
  private
    FState: Pointer;
    FOutBuf: array of Byte;
    FInBuf: array of Byte;
    FInUsed: NativeInt;

    FQuality: Integer;
    FLgWin: Integer;
    FMode: BrotliEncoderMode;
    FFinished: Boolean;

    procedure EnsureState;
    procedure EncodeChunk(const Op: BrotliEncoderOperation; const InData: PByte;
      InSize: size_t);
    procedure FlushEncoder(const Op: BrotliEncoderOperation);
  protected
    function GetSize: Int64; override;
  public
    constructor Create(Dest: TStream; Quality: Integer = 11;
      LgWin: Integer = 22; Mode: BrotliEncoderMode = BROTLI_MODE_GENERIC;
      AOptions: TBrotliStreamOptions = []);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;

    procedure Flush;
    procedure Finish;
  end;

  TBrotliDecompressionStream = class(TCustomBrotliStream)
  private
    FState: Pointer;

    FInBuf: array of Byte;
    FInPos: NativeInt;
    FInAvail: NativeInt;

    FOutBuf: array of Byte;
    FOutPos: NativeInt;
    FOutAvail: NativeInt;

    FFinished: Boolean;

    procedure EnsureState;
    procedure RefillInput;
    procedure ProduceMoreOutput;
  protected
    function GetSize: Int64; override;
  public
    constructor Create(Source: TStream; AOptions: TBrotliStreamOptions = []);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

function GetMaxCompressedSize(const Count: size_t): size_t;

function BrotliCompressBuf(InBuf: PByte; InBytesCount: size_t;
  out OutBuf: PByte; out OutBytesCount: size_t): size_t;

function BrotliDecompress(const compressedBuffer; compressedBuffSize: size_t;
  var DecBuffer: PByte; var decSize: Psize_t): Longint;

function BrotliVersionToString(const Ver: TBrotliVersion): string;
function BrotliDecoderVersionString: string;
function BrotliEncoderVersionString: string;

implementation

function brAllocMem(AppData: Pointer; Count: Integer): Pointer; cdecl;
begin
  GetMem(Result, Count);
end;

procedure brFreeMem(Ptr, Size: Pointer); cdecl;
begin
  FreeMem(Ptr);
end;

{ ----------------- helpers / simple buf api ----------------- }

function GetMaxCompressedSize(const Count: size_t): size_t;
begin
  Result := BrotliEncoderMaxCompressedSize(Count);
end;

function BrotliCompressBuf(InBuf: PByte; InBytesCount: size_t;
  out OutBuf: PByte; out OutBytesCount: size_t): size_t;
var
  cap: size_t;
  ok: Longint;
begin
  OutBuf := nil;
  OutBytesCount := 0;
  Result := 0;

  if (InBuf = nil) or (InBytesCount = 0) then
    Exit;

  cap := BrotliEncoderMaxCompressedSize(InBytesCount);
  GetMem(OutBuf, cap);
  OutBytesCount := cap;

  ok := BrotliEncoderCompress(11, 22, BROTLI_MODE_GENERIC, InBytesCount, InBuf,
    @OutBytesCount, OutBuf);

  if ok = BROTLI_FALSE then
  begin
    FreeMem(OutBuf);
    OutBuf := nil;
    OutBytesCount := 0;
    raise EBrotliCompressionError.Create('BrotliEncoderCompress failed');
  end;

  ReallocMem(OutBuf, OutBytesCount);
  Result := OutBytesCount;
end;

function BrotliDecompress(const compressedBuffer; compressedBuffSize: size_t;
  var DecBuffer: PByte; var decSize: Psize_t): Longint;
var
  st: Pointer;
  inPtr: PByte;
  availIn: size_t;

  outChunk: array [0 .. 65535] of Byte;
  outPtr: PByte;
  availOut: size_t;

  res: BrotliDecoderResult;
  produced: size_t;
  writePos: size_t;
begin
  DecBuffer := nil;
  if Assigned(decSize) then
    decSize^ := 0;
  Result := BROTLI_FALSE;

  if compressedBuffSize = 0 then
    Exit;

  st := BrotliDecoderCreateInstance(nil, nil, nil);
  if st = nil then
    Exit;

  try
    inPtr := @compressedBuffer;
    availIn := compressedBuffSize;
    writePos := 0;

    repeat
      outPtr := @outChunk[0];
      availOut := SizeOf(outChunk);

      res := BrotliDecoderDecompressStream(st, @availIn, @inPtr, @availOut,
        @outPtr, nil);

      produced := size_t(SizeOf(outChunk)) - availOut;
      if produced <> 0 then
      begin
        ReallocMem(DecBuffer, writePos + produced);
        Move(outChunk[0], (DecBuffer + writePos)^, produced);
        Inc(writePos, produced);
      end;

      if res = BROTLI_DECODER_RESULT_ERROR then
        Exit(BROTLI_FALSE);

      if (res = BROTLI_DECODER_RESULT_NEEDS_MORE_INPUT) and (availIn = 0) then
        Exit(BROTLI_FALSE);

    until res = BROTLI_DECODER_RESULT_SUCCESS;

    if Assigned(decSize) then
      decSize^ := writePos;
    Result := BROTLI_TRUE;
  finally
    BrotliDecoderDestroyInstance(st);
  end;
end;

function BrotliVersionToString(const Ver: TBrotliVersion): string;
begin
  Result := Format('%d.%d.%d', [Ver.Major, Ver.Minor, Ver.Patch]);
end;

function BrotliDecoderVersionString: string;
begin
  Result := BrotliVersionToString(BrotliVersionDecode(BrotliDecoderVersion));
end;

function BrotliEncoderVersionString: string;
begin
  Result := BrotliVersionToString(BrotliVersionDecode(BrotliEncoderVersion));
end;

{ ----------------- TCustomBrotliStream ----------------- }

constructor TCustomBrotliStream.Create(Strm: TStream;
  Options: TBrotliStreamOptions);
begin
  inherited Create;
  FStrm := Strm;
  FStrmPos := Strm.Position;
  FOptions := Options;
end;

procedure TCustomBrotliStream.Progress(Sender: TObject);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender);
end;

{ ----------------- TBrotliCompressionStream ----------------- }

constructor TBrotliCompressionStream.Create(Dest: TStream; Quality: Integer;
  LgWin: Integer; Mode: BrotliEncoderMode; AOptions: TBrotliStreamOptions);
begin
  inherited Create(Dest, AOptions);
  FState := nil;

  FQuality := Quality;
  if FQuality < 0 then
    FQuality := 0
  else if FQuality > 11 then
    FQuality := 11;

  FLgWin := LgWin;
  if FLgWin < 10 then
    FLgWin := 10
  else if FLgWin > 24 then
    FLgWin := 24;

  FMode := Mode;

  SetLength(FInBuf, BROTLI_BUFFER_SIZE);
  SetLength(FOutBuf, BROTLI_BUFFER_SIZE);
  FInUsed := 0;
  FFinished := false;

  EnsureState;
end;

destructor TBrotliCompressionStream.Destroy;
begin
  try
    Finish;
  except
    // destructor must not raise
  end;

  if FState <> nil then
  begin
    BrotliEncoderDestroyInstance(FState);
    FState := nil;
  end;

  if not(brLeaveOpen in Options) then
    BaseStream.Free;

  inherited;
end;

procedure TBrotliCompressionStream.EnsureState;
begin
  if FState <> nil then
    Exit;

  FState := BrotliEncoderCreateInstance(nil, nil, nil);
  if FState = nil then
    raise EBrotliCompressionError.Create('BrotliEncoderCreateInstance failed');

  // First up MODE
  if BrotliEncoderSetParameter(FState, BROTLI_PARAM_MODE, uint32_t(Ord(FMode)))
    = BROTLI_FALSE then
    raise EBrotliCompressionError.Create
      ('BrotliEncoderSetParameter(MODE) failed');

  // enable LARGE_WINDOW
  if FLgWin >= 16 then
  begin
    if BrotliEncoderSetParameter(FState, BROTLI_PARAM_LARGE_WINDOW, 1) = BROTLI_FALSE
    then
      raise EBrotliCompressionError.Create
        ('BrotliEncoderSetParameter(LARGE_WINDOW) failed');
  end;

  // Then LGWIN
  if BrotliEncoderSetParameter(FState, BROTLI_PARAM_LGWIN, uint32_t(FLgWin)) = BROTLI_FALSE
  then
    raise EBrotliCompressionError.CreateFmt
      ('BrotliEncoderSetParameter(LGWIN=%d) failed', [FLgWin]);

  // At last QUALITY
  if BrotliEncoderSetParameter(FState, BROTLI_PARAM_QUALITY, uint32_t(FQuality))
    = BROTLI_FALSE then
    raise EBrotliCompressionError.Create
      ('BrotliEncoderSetParameter(QUALITY) failed');
end;

procedure TBrotliCompressionStream.EncodeChunk(const Op: BrotliEncoderOperation;
  const InData: PByte; InSize: size_t);
var
  availIn, availOut: size_t;
  nextIn, nextOut: PByte;
  totalOut: size_t;
  ok: BROTLI_BOOL;
  produced: size_t;
begin
  availIn := InSize;
  nextIn := InData;

  repeat
    availOut := size_t(Length(FOutBuf));
    nextOut := @FOutBuf[0];
    totalOut := 0;

    ok := BrotliEncoderCompressStream(FState, Op, @availIn, @nextIn, @availOut,
      @nextOut, @totalOut);

    if ok = BROTLI_FALSE then
      raise EBrotliCompressionError.Create
        ('BrotliEncoderCompressStream failed');

    produced := size_t(Length(FOutBuf)) - availOut;
    if produced <> 0 then
      BaseStream.WriteBuffer(FOutBuf[0], produced);

    Progress(Self);
  until (availIn = 0) and (BrotliEncoderHasMoreOutput(FState) = BROTLI_FALSE);
end;

procedure TBrotliCompressionStream.FlushEncoder
  (const Op: BrotliEncoderOperation);
begin
  // Call with no input to drain internal buffers for Flush/Finish
  EncodeChunk(Op, nil, 0);
end;

function TBrotliCompressionStream.Write(const Buffer; Count: Longint): Longint;
var
  p: PByte;
  take: NativeInt;
begin
  Result := 0;
  if Count <= 0 then
    Exit;

  EnsureState;

  p := @Buffer;
  while Count > 0 do
  begin
    take := Length(FInBuf) - FInUsed;
    if take > Count then
      take := Count;

    Move(p^, FInBuf[FInUsed], take);
    Inc(FInUsed, take);
    Inc(p, take);
    Dec(Count, take);
    Inc(Result, take);

    if FInUsed = Length(FInBuf) then
    begin
      EncodeChunk(BROTLI_OPERATION_PROCESS, @FInBuf[0], size_t(FInUsed));
      FInUsed := 0;
    end;
  end;
end;

function TBrotliCompressionStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := 0;
  raise EBrotliCompressionError.Create
    ('TBrotliCompressionStream is write-only');
end;

procedure TBrotliCompressionStream.Flush;
begin
  EnsureState;

  if FInUsed <> 0 then
  begin
    EncodeChunk(BROTLI_OPERATION_PROCESS, @FInBuf[0], size_t(FInUsed));
    FInUsed := 0;
  end;

  FlushEncoder(BROTLI_OPERATION_FLUSH);
end;

procedure TBrotliCompressionStream.Finish;
begin
  if FFinished then
    Exit;
  if FState = nil then
    Exit;

  if FInUsed <> 0 then
  begin
    EncodeChunk(BROTLI_OPERATION_PROCESS, @FInBuf[0], size_t(FInUsed));
    FInUsed := 0;
  end;

  FlushEncoder(BROTLI_OPERATION_FINISH);

  // Ensure encoder finished
  if BrotliEncoderIsFinished(FState) = BROTLI_FALSE then
    FlushEncoder(BROTLI_OPERATION_FINISH);
  FFinished := true;
end;

function TBrotliCompressionStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  // Like zlib streams: you can't seek within compressed data reliably.
  if (Offset = 0) and (Origin = soCurrent) then
    Exit(0);

  raise EBrotliCompressionError.Create
    ('Seek not supported on TBrotliCompressionStream');
end;

function TBrotliCompressionStream.GetSize: Int64;
begin
  Result := 0;
end;

{ ----------------- TBrotliDecompressionStream ----------------- }

constructor TBrotliDecompressionStream.Create(Source: TStream;
  AOptions: TBrotliStreamOptions);
begin
  inherited Create(Source, AOptions);
  FState := nil;

  SetLength(FInBuf, BROTLI_BUFFER_SIZE);
  SetLength(FOutBuf, BROTLI_BUFFER_SIZE);

  FInPos := 0;
  FInAvail := 0;
  FOutPos := 0;
  FOutAvail := 0;
  FFinished := false;

  EnsureState;
end;

destructor TBrotliDecompressionStream.Destroy;
begin
  if FState <> nil then
  begin
    BrotliDecoderDestroyInstance(FState);
    FState := nil;
  end;

  if not(brLeaveOpen in Options) then
    BaseStream.Free;

  inherited;
end;

procedure TBrotliDecompressionStream.EnsureState;
begin
  if FState <> nil then
    Exit;

  FState := BrotliDecoderCreateInstance(nil, nil, nil);
  if FState = nil then
    raise EBrotliDecompressionError.Create
      ('BrotliDecoderCreateInstance failed');

  if BrotliDecoderSetParameter(FState, BROTLI_DECODER_PARAM_LARGE_WINDOW, 1) = BROTLI_FALSE
  then
    raise EBrotliDecompressionError.Create
      ('BrotliDecoderSetParameter(LARGE_WINDOW) failed');
end;

procedure TBrotliDecompressionStream.RefillInput;
begin
  if FInPos < FInAvail then
    Exit;

  FInPos := 0;
  FInAvail := BaseStream.Read(FInBuf[0], Length(FInBuf));
end;

procedure TBrotliDecompressionStream.ProduceMoreOutput;
var
  availIn: size_t;
  nextIn: PByte;
  availOut: size_t;
  nextOut: PByte;
  res: BrotliDecoderResult;
  produced: size_t;
  eof: Boolean;
begin
  if FFinished then
    Exit;

  FOutPos := 0;
  FOutAvail := 0;

  eof := false;

  while FOutAvail = 0 do
  begin
    // ensure we have some input if possible
    if FInPos >= FInAvail then
    begin
      FInPos := 0;
      FInAvail := BaseStream.Read(FInBuf[0], Length(FInBuf));
      eof := (FInAvail = 0);
    end;

    availIn := size_t(FInAvail - FInPos);
    nextIn := @FInBuf[FInPos];

    // try to produce output; if decoder wants more output, we just loop again
    availOut := size_t(Length(FOutBuf));
    nextOut := @FOutBuf[0];

    res := BrotliDecoderDecompressStream(
      FState,
      @availIn, @nextIn,
      @availOut, @nextOut,
      nil
    );

    // update consumed input position
    FInPos := NativeInt(NativeUInt(nextIn) - NativeUInt(@FInBuf[0]));

    produced := size_t(Length(FOutBuf)) - availOut;
    if produced <> 0 then
    begin
      FOutAvail := NativeInt(produced);
      Exit; // give produced bytes to Read()
    end;

    case res of
      BROTLI_DECODER_RESULT_SUCCESS:
        begin
          FFinished := true;
          Exit;
        end;

      BROTLI_DECODER_RESULT_ERROR:
        raise EBrotliDecompressionError.CreateFmt('Brotli decode error: %s',
          [string(AnsiString(
            BrotliDecoderErrorString(BrotliDecoderGetErrorCode(FState))
          ))]);

      BROTLI_DECODER_RESULT_NEEDS_MORE_INPUT:
        begin
          // If EOF and still needs input -> truncated
          if eof then
            raise EBrotliDecompressionError.Create
              ('Brotli stream truncated (needs more input)');
          // else loop: RefillInput on next iteration
        end;

      BROTLI_DECODER_RESULT_NEEDS_MORE_OUTPUT:
        begin
          // We provided a full output buffer; if produced=0 here, just loop.
          // (Next iteration uses a fresh outbuf again.)
        end;
    end;
  end;
end;

function TBrotliDecompressionStream.Read(var Buffer; Count: Longint): Longint;
var
  p: PByte;
  take: NativeInt;
begin
  Result := 0;
  if Count <= 0 then
    Exit;

  p := @Buffer;

  while Count > 0 do
  begin
    if FOutPos >= FOutAvail then
    begin
      ProduceMoreOutput;
      if (FOutAvail = 0) and FFinished then
        Break; // EOF
    end;

    take := FOutAvail - FOutPos;
    if take > Count then
      take := Count;

    Move(FOutBuf[FOutPos], p^, take);
    Inc(FOutPos, take);
    Inc(p, take);
    Dec(Count, take);
    Inc(Result, take);
  end;
end;

function TBrotliDecompressionStream.Write(const Buffer; Count: Longint)
  : Longint;
begin
  Result := 0;
  raise EBrotliDecompressionError.Create
    ('TBrotliDecompressionStream is read-only');
end;

function TBrotliDecompressionStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  if (Offset = 0) and (Origin = soCurrent) then
    Exit(0);

  raise EBrotliDecompressionError.Create
    ('Seek not supported on TBrotliDecompressionStream');
end;

function TBrotliDecompressionStream.GetSize: Int64;
begin
  Result := 0;
end;

end.
