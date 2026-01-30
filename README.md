# Brotli Bindings and Streams for Delphi and Free Pascal

## Overview

This package provides complete Brotli compression/decompression support for **Delphi** (Windows 32/64-bit) and **Free Pascal** (Windows 32/64-bit and Linux 64-bit).

It consists of two units:

- **`libbrotli.pas`** – Low-level direct bindings to the official Brotli C library (`libbrotlicommon`, `libbrotlienc`, `libbrotlidec`).
- **`brotli.pas`** – High-level stream classes (`TBrotliCompressionStream`, `TBrotliDecompressionStream`) and helper functions for easy integration into Delphi/FPC applications.

The package includes pre-compiled static libraries and object files:
- Windows (Delphi/FPC): `.obj` files linked directly via `{$L ...}`
- Linux (FPC): `.a` static libraries
- All necessary C sources from the official Brotli release are compiled and included.

Tested platforms:
- Delphi (10.4 Sydney and later) – Windows 64-bit
- Lazarus/FPC – Windows 64-bit and Linux 64-bit

Delphi on Linux/macOS should work in principle (via static linking), but has not been tested (no FMX Linux environment available).

## libbrotli.pas – Low-Level Bindings

This unit provides direct Pascal declarations for the entire Brotli C API.

### Key Features

- Full access to encoder and decoder functions (`BrotliEncoderCompress`, `BrotliDecoderDecompressStream`, etc.)
- All types, constants, enums, and structures from `brotli/decode.h` and `brotli/encode.h`
- Custom memory manager support via callbacks
- Version helpers (`BrotliDecoderVersion`, `BrotliEncoderVersion`, `BrotliVersionDecode`)

### Usage Example (simple buffer compression)

```pascal
var
  Input, Output: PByte;
  InSize, OutSize: size_t;
  Success: Longint;
begin
  InSize := ...;
  GetMem(Input, InSize);
  // fill Input...

  OutSize := BrotliEncoderMaxCompressedSize(InSize);
  GetMem(Output, OutSize);

  Success := BrotliEncoderCompress(
    11,                           // quality (0-11)
    22,                           // lgwin (10-24)
    BROTLI_MODE_GENERIC,
    InSize, Input,
    @OutSize, Output);

  if Success = BROTLI_TRUE then
    // Output contains compressed data, OutSize is actual size
  else
    // compression failed
end;
```

Most users will prefer the higher-level streams in `brotli.pas`.

## brotli.pas – High-Level Streams and Helpers

This unit provides easy-to-use stream classes similar to `TZCompressionStream`/`TDecompressionStream` from ZLib.

### TBrotliCompressionStream

Write-only stream that compresses data written to it and outputs to an underlying stream.

#### Constructor

```pascal
constructor Create(Dest: TStream;
  Quality: Integer = 11;
  LgWin: Integer = 22;
  Mode: BrotliEncoderMode = BROTLI_MODE_GENERIC;
  AOptions: TBrotliStreamOptions = []);
```

- **Quality**: 0–11 (higher = better compression, slower). Default 11.
- **LgWin**: Log2 of window size, 10–24. Default 22.
- **Mode**: `BROTLI_MODE_GENERIC` (default), `BROTLI_MODE_TEXT`, or `BROTLI_MODE_FONT`.
- **AOptions**: 
  - `brLeaveOpen` – do not free the destination stream in destructor.

#### Key Methods

- `Write` – compresses data
- `Flush` – flushes internal buffers (calls Brotli FLUSH operation)
- `Finish` – completes compression (calls Brotli FINISH). Called automatically in destructor.

#### Example

```pascal
var
  Source, Compressed: TFileStream;
begin
  Source := TFileStream.Create('input.txt', fmOpenRead);
  Compressed := TFileStream.Create('input.br', fmCreate);
  try
    with TBrotliCompressionStream.Create(Compressed, 11, 22, BROTLI_MODE_TEXT) do
    try
      CopyFrom(Source, 0);
      Finish;  // important before closing
    finally
      Free;
    end;
  finally
    Source.Free;
  end;
end;
```

### TBrotliDecompressionStream

Read-only stream that decompresses data from an underlying stream.

#### Constructor

```pascal
constructor Create(Source: TStream; AOptions: TBrotliStreamOptions = []);
```

- Supports large window (`BROTLI_PARAM_LARGE_WINDOW`) automatically enabled.
- `brLeaveOpen` – do not free source stream in destructor.

#### Example

```pascal
var
  Compressed, Output: TFileStream;
begin
  Compressed := TFileStream.Create('input.br', fmOpenRead);
  Output := TFileStream.Create('output.txt', fmCreate);
  try
    with TBrotliDecompressionStream.Create(Compressed) do
    try
      Output.CopyFrom(Self, 0);
    finally
      Free;
    end;
  finally
    Output.Free;
  end;
end;
```

### Helper Functions

```pascal
function GetMaxCompressedSize(const Count: size_t): size_t;
// Returns worst-case compressed size

function BrotliCompressBuf(InBuf: PByte; InBytesCount: size_t;
  out OutBuf: PByte; out OutBytesCount: size_t): size_t;
// One-shot compression (quality 11, lgwin 22, generic mode)
// Allocates and returns compressed buffer, raises exception on failure

function BrotliDecompress(const compressedBuffer; compressedBuffSize: size_t;
  var DecBuffer: PByte; var decSize: Psize_t): Longint;
// One-shot decompression of entire buffer
// Allocates DecBuffer, returns BROTLI_TRUE/BROTLI_FALSE

function BrotliDecoderVersionString: string;
function BrotliEncoderVersionString: string;
// Human-readable version strings, e.g., "1.0.9"
```

### Error Handling

- `EBrotliError` – base class
- `EBrotliCompressionError` – compression failures
- `EBrotliDecompressionError` – decompression failures (includes error message from decoder when possible)

## Installation / Usage

1. Add both units (`libbrotli.pas` and `brotli.pas`) to your project.
2. No external DLLs required – everything is statically linked.
3. For FPC on Linux: the `.a` libraries are automatically linked via `{$linklib ...}` directives.
4. For Delphi on Windows: object files are linked via `{$L ...}`.

No additional setup needed.

## License

The Brotli library itself is under the MIT License.  
The Pascal bindings and wrapper code are provided under the same permissive terms – feel free to use in any project (commercial or open-source).

Enjoy fast and efficient compression with Brotli in your Delphi and Lazarus applications!
