{
  Copyright (c) 2026 Aleksandr Vorobev aka CynicRus, CynicRus@gmail.com

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}
unit libbrotli;
{$IFDEF FPC}
{$PACKRECORDS C}
{$mode Delphi}
{$ENDIF}
{$DEFINE BROTLI_ENCODER_EXIT_ON_OOM}

interface

uses
{$IFNDEF FPC}
{$IFNDEF DELPHI_7}
  System.Classes, System.SysUtils {$IFDEF MSWINDOWS},
  Winapi.Windows, System.Math{$ENDIF};
{$ELSE}
  Windows, Classes, SysUtils;
{$ENDIF}
{$ENDIF}
{$IFDEF FPC}
 Classes, SysUtils, LCLIntf;
{$ENDIF}

{$IFNDEF FPC}
  {$Z4}
{$ENDIF}

const
  SHARED_BROTLI_MIN_DICTIONARY_WORD_LENGTH = 4;
  SHARED_BROTLI_MAX_DICTIONARY_WORD_LENGTH = 31;
  SHARED_BROTLI_NUM_DICTIONARY_CONTEXTS = 64;
  SHARED_BROTLI_MAX_COMPOUND_DICTS = 15;
  BROTLI_HUFFMAN_MAX_CODE_LENGTH = 15;
  BROTLI_HUFFMAN_MAX_SIZE_26 = 396;
  BROTLI_HUFFMAN_MAX_SIZE_258 = 632;
  BROTLI_HUFFMAN_MAX_SIZE_272 = 646;
  BROTLI_HUFFMAN_MAX_CODE_LENGTH_CODE_LENGTH = 5;
  BROTLI_REPEAT_ZERO_CODE_LENGTH = 17;
  BROTLI_CODE_LENGTH_CODES = 18;
  BROTLI_NUM_LITERAL_SYMBOLS = 256;
  BROTLI_NUM_COMMAND_SYMBOLS = 704;
  BROTLI_NUM_BLOCK_LEN_SYMBOLS = 26;

const
  BROTLI_TRUE = 1;
  BROTLI_FALSE = 0;

type
{$IFNDEF FPC}
  {$IFDEF LINUX}
    BROTLI_BOOL = FixedInt; 
  {$ELSE}
    BROTLI_BOOL = LongInt;
  {$ENDIF}
{$ELSE}
  BROTLI_BOOL = LongInt;
{$ENDIF}

type
  int8_t = ShortInt;
  uint8_t = Byte;
  int16_t = SmallInt;
  uint16_t = Word;
  int32_t = longint;
  uint32_t = LongWord;
  int64_t = Int64;
  uint64_t = UInt64;

  size_t = NativeUInt;
  Psize_t = ^size_t;

  ppbyte = ^PByte;

  tbrotli_alloc_func = function(opaque: pointer; size: size_t): pointer; cdecl;

  tbrotli_free_func = procedure(opaque: pointer; address: pointer); cdecl;

type

  tbrotli_decoder_metadata_start_func = procedure(opaque: pointer;
    size: size_t); cdecl;

  tbrotli_decoder_metadata_chunk_func = procedure(opaque: pointer; data: PByte;
    size: size_t); cdecl;

type
  BrotliWordTransformType = (BROTLI_TRANSFORM_IDENTITY = 0,
    BROTLI_TRANSFORM_OMIT_LAST_1 = 1, BROTLI_TRANSFORM_OMIT_LAST_2 = 2,
    BROTLI_TRANSFORM_OMIT_LAST_3 = 3, BROTLI_TRANSFORM_OMIT_LAST_4 = 4,
    BROTLI_TRANSFORM_OMIT_LAST_5 = 5, BROTLI_TRANSFORM_OMIT_LAST_6 = 6,
    BROTLI_TRANSFORM_OMIT_LAST_7 = 7, BROTLI_TRANSFORM_OMIT_LAST_8 = 8,
    BROTLI_TRANSFORM_OMIT_LAST_9 = 9, BROTLI_TRANSFORM_UPPERCASE_FIRST = 10,
    BROTLI_TRANSFORM_UPPERCASE_ALL = 11, BROTLI_TRANSFORM_OMIT_FIRST_1 = 12,
    BROTLI_TRANSFORM_OMIT_FIRST_2 = 13, BROTLI_TRANSFORM_OMIT_FIRST_3 = 14,
    BROTLI_TRANSFORM_OMIT_FIRST_4 = 15, BROTLI_TRANSFORM_OMIT_FIRST_5 = 16,
    BROTLI_TRANSFORM_OMIT_FIRST_6 = 17, BROTLI_TRANSFORM_OMIT_FIRST_7 = 18,
    BROTLI_TRANSFORM_OMIT_FIRST_8 = 19, BROTLI_TRANSFORM_OMIT_FIRST_9 = 20,
    BROTLI_TRANSFORM_SHIFT_FIRST = 21, BROTLI_TRANSFORM_SHIFT_ALL = 22);

  BrotliTransforms = packed record
  var
    prefix_suffix_size: uint16;
    prefix_suffix: PByte;
    prefix_suffix_map: ^Word;
    num_transforms: UInt32;
    transforms: PByte;
    params: PByte;
    cutOffTransforms: array [0 .. 9] of int16;
  end;

  PBrotliTransforms = ^BrotliTransforms;

type
  BrotliDictionary = packed record
  var
    size_bits_by_length: array [0 .. 31] of Byte;
    offsets_by_length: array [0 .. 31] of UInt32;
    data_size: size_t;
    data: PByte;
  end;

  PBrotliDictionary = ^BrotliDictionary;

  BrotliSharedDictionaryStruct = packed record
  var
    num_prefix: UInt32;
    prefix_size: array [0 .. (SHARED_BROTLI_MAX_COMPOUND_DICTS) - 1] of size_t;
    context_based: BROTLI_BOOL;
    context_map: array [0 .. (SHARED_BROTLI_NUM_DICTIONARY_CONTEXTS) -
      1] of Byte;
    num_dictionaries, num_word_lists: Byte;
    words_instances: PBrotliDictionary;
    num_transform_lists: Byte;
    transforms_instances: BrotliTransforms;
    prefix_suffix_maps: uint16_t;
    alloc_func: tbrotli_alloc_func;
    free_func: tbrotli_free_func;
    memory_manager_opaque: pointer;
  end;

  PBrotliSharedDictionaryStruct = ^BrotliSharedDictionaryStruct;

type

  BrotliSharedDictionaryType = (BROTLI_SHARED_DICTIONARY_RAW = 0,
    BROTLI_SHARED_DICTIONARY_SERIALIZED = 1);

type
  BrotliBitReader = packed record
  var
    val_: UInt64;
    bit_pos_: UInt32;
    next_in: PByte;
    avail_in: size_t;
  end;

  PBrotliBitReader = ^BrotliBitReader;

type
  BrotliBitReaderState = packed record
  var
    val_: UInt64;
    bit_pos_: UInt32;
    next_in: PByte;
    avail_in: size_t;
  end;

  PBrotliBitReaderState = ^BrotliBitReaderState;

function TO_BROTLI_BOOL(X: longint): longint;

function BROTLI_MAKE_UINT64_T(high, low: longint): longint;

function BROTLI_UINT32_MAX: longint;

function BROTLI_SIZE_MAX: longint;

type
  BrotliRunningState = (BROTLI_STATE_UNINITED, BROTLI_STATE_LARGE_WINDOW_BITS,
    BROTLI_STATE_INITIALIZE, BROTLI_STATE_METABLOCK_BEGIN,
    BROTLI_STATE_METABLOCK_HEADER, BROTLI_STATE_METABLOCK_HEADER_2,
    BROTLI_STATE_CONTEXT_MODES, BROTLI_STATE_COMMAND_BEGIN,
    BROTLI_STATE_COMMAND_INNER, BROTLI_STATE_COMMAND_POST_DECODE_LITERALS,
    BROTLI_STATE_COMMAND_POST_WRAP_COPY, BROTLI_STATE_UNCOMPRESSED,
    BROTLI_STATE_METADATA, BROTLI_STATE_COMMAND_INNER_WRITE,
    BROTLI_STATE_METABLOCK_DONE, BROTLI_STATE_COMMAND_POST_WRITE_1,
    BROTLI_STATE_COMMAND_POST_WRITE_2,
    BROTLI_STATE_BEFORE_COMPRESSED_METABLOCK_HEADER,
    BROTLI_STATE_HUFFMAN_CODE_0, BROTLI_STATE_HUFFMAN_CODE_1,
    BROTLI_STATE_HUFFMAN_CODE_2, BROTLI_STATE_HUFFMAN_CODE_3,
    BROTLI_STATE_CONTEXT_MAP_1, BROTLI_STATE_CONTEXT_MAP_2,
    BROTLI_STATE_TREE_GROUP, BROTLI_STATE_BEFORE_COMPRESSED_METABLOCK_BODY,
    BROTLI_STATE_DONE);

  BrotliDecoderResult = (BROTLI_DECODER_RESULT_ERROR = 0,
    BROTLI_DECODER_RESULT_SUCCESS = 1,
    BROTLI_DECODER_RESULT_NEEDS_MORE_INPUT = 2,
    BROTLI_DECODER_RESULT_NEEDS_MORE_OUTPUT = 3);

  BrotliDecoderErrorCode = (BROTLI_DECODER_NO_ERROR = 0,
    BROTLI_DECODER_SUCCESS = 1, BROTLI_DECODER_NEEDS_MORE_INPUT = 2,
    BROTLI_DECODER_NEEDS_MORE_OUTPUT = 3,
    BROTLI_DECODER_ERROR_FORMAT_EXUBERANT_NIBBLE = -(1),
    BROTLI_DECODER_ERROR_FORMAT_RESERVED = -(2),
    BROTLI_DECODER_ERROR_FORMAT_EXUBERANT_META_NIBBLE = -(3),
    BROTLI_DECODER_ERROR_FORMAT_SIMPLE_HUFFMAN_ALPHABET = -(4),
    BROTLI_DECODER_ERROR_FORMAT_SIMPLE_HUFFMAN_SAME = -(5),
    BROTLI_DECODER_ERROR_FORMAT_CL_SPACE = -(6),
    BROTLI_DECODER_ERROR_FORMAT_HUFFMAN_SPACE = -(7),
    BROTLI_DECODER_ERROR_FORMAT_CONTEXT_MAP_REPEAT = -(8),
    BROTLI_DECODER_ERROR_FORMAT_BLOCK_LENGTH_1 = -(9),
    BROTLI_DECODER_ERROR_FORMAT_BLOCK_LENGTH_2 = -(10),
    BROTLI_DECODER_ERROR_FORMAT_TRANSFORM = -(11),
    BROTLI_DECODER_ERROR_FORMAT_DICTIONARY = -(12),
    BROTLI_DECODER_ERROR_FORMAT_WINDOW_BITS = -(13),
    BROTLI_DECODER_ERROR_FORMAT_PADDING_1 = -(14),
    BROTLI_DECODER_ERROR_FORMAT_PADDING_2 = -(15),
    BROTLI_DECODER_ERROR_FORMAT_DISTANCE = -(16),
    BROTLI_DECODER_ERROR_COMPOUND_DICTIONARY = -(18),
    BROTLI_DECODER_ERROR_DICTIONARY_NOT_SET = -(19),
    BROTLI_DECODER_ERROR_INVALID_ARGUMENTS = -(20),
    BROTLI_DECODER_ERROR_ALLOC_CONTEXT_MODES = -(21),
    BROTLI_DECODER_ERROR_ALLOC_TREE_GROUPS = -(22),
    BROTLI_DECODER_ERROR_ALLOC_CONTEXT_MAP = -(25),
    BROTLI_DECODER_ERROR_ALLOC_RING_BUFFER_1 = -(26),
    BROTLI_DECODER_ERROR_ALLOC_RING_BUFFER_2 = -(27),
    BROTLI_DECODER_ERROR_ALLOC_BLOCK_TYPE_TREES = -(30),
    BROTLI_DECODER_ERROR_UNREACHABLE = -(31));

  BrotliRunningMetablockHeaderState = (BROTLI_STATE_METABLOCK_HEADER_NONE,
    BROTLI_STATE_METABLOCK_HEADER_EMPTY, BROTLI_STATE_METABLOCK_HEADER_NIBBLES,
    BROTLI_STATE_METABLOCK_HEADER_SIZE,
    BROTLI_STATE_METABLOCK_HEADER_UNCOMPRESSED,
    BROTLI_STATE_METABLOCK_HEADER_RESERVED, BROTLI_STATE_METABLOCK_HEADER_BYTES,
    BROTLI_STATE_METABLOCK_HEADER_METADATA);

  BrotliRunningUncompressedState = (BROTLI_STATE_UNCOMPRESSED_NONE,
    BROTLI_STATE_UNCOMPRESSED_WRITE);

  BrotliRunningTreeGroupState = (BROTLI_STATE_TREE_GROUP_NONE,
    BROTLI_STATE_TREE_GROUP_LOOP);

  BrotliRunningContextMapState = (BROTLI_STATE_CONTEXT_MAP_NONE,
    BROTLI_STATE_CONTEXT_MAP_READ_PREFIX, BROTLI_STATE_CONTEXT_MAP_HUFFMAN,
    BROTLI_STATE_CONTEXT_MAP_DECODE, BROTLI_STATE_CONTEXT_MAP_TRANSFORM);

  BrotliRunningHuffmanState = (BROTLI_STATE_HUFFMAN_NONE,
    BROTLI_STATE_HUFFMAN_SIMPLE_SIZE, BROTLI_STATE_HUFFMAN_SIMPLE_READ,
    BROTLI_STATE_HUFFMAN_SIMPLE_BUILD, BROTLI_STATE_HUFFMAN_COMPLEX,
    BROTLI_STATE_HUFFMAN_LENGTH_SYMBOLS);

  BrotliRunningDecodeUint8State = (BROTLI_STATE_DECODE_UINT8_NONE,
    BROTLI_STATE_DECODE_UINT8_SHORT, BROTLI_STATE_DECODE_UINT8_LONG);

  BrotliRunningReadBlockLengthState = (BROTLI_STATE_READ_BLOCK_LENGTH_NONE,
    BROTLI_STATE_READ_BLOCK_LENGTH_SUFFIX);

  BrotliDecoderParameter =
    (BROTLI_DECODER_PARAM_DISABLE_RING_BUFFER_REALLOCATION = 0,
    BROTLI_DECODER_PARAM_LARGE_WINDOW = 1);

  HuffmanCode = packed record
    bits: uint8_t;
    value: uint16_t;
  end;

  PHuffmanCode = ^HuffmanCode;

  HuffmanTreeGroup = packed record
    htrees: ^PHuffmanCode;
    codes: ^HuffmanCode;
    alphabet_size_max: uint16_t;
    alphabet_size_limit: uint16_t;
    num_htrees: uint16_t;
  end;

  BrotliMetablockHeaderArena = packed record
    substate_tree_group: BrotliRunningTreeGroupState;
    substate_context_map: BrotliRunningContextMapState;
    substate_huffman: BrotliRunningHuffmanState;
    sub_loop_counter: uint32_t;
    repeat_code_len: uint32_t;
    prev_code_len: uint32_t;
    symbol: uint32_t;
    _repeat: uint32_t;
    space: uint32_t;
    table: array [0 .. 31] of HuffmanCode;
    symbol_lists: ^uint16_t;
    symbols_lists_array
      : array [0 .. ((BROTLI_HUFFMAN_MAX_CODE_LENGTH + 1) +
      BROTLI_NUM_COMMAND_SYMBOLS) - 1] of uint16_t;
    next_symbol: array [0 .. 31] of longint;
    code_length_code_lengths: array [0 .. (BROTLI_CODE_LENGTH_CODES) - 1]
      of uint8_t;
    code_length_histo: array [0 .. 15] of uint16_t;
    htree_index: longint;
    next: ^HuffmanCode;
    context_index: uint32_t;
    max_run_length_prefix: uint32_t;
    code: uint32_t;
    context_map_table: array [0 .. (BROTLI_HUFFMAN_MAX_SIZE_272) - 1]
      of HuffmanCode;
  end;

  BrotliDecoderCompoundDictionary = packed record
    num_chunks: longint;
    total_size: longint;
    br_index: longint;
    br_offset: longint;
    br_length: longint;
    br_copied: longint;
    chunks: array [0 .. 15] of PByte;
    chunk_offsets: array [0 .. 15] of longint;
    block_bits: longint;
    block_map: array [0 .. 255] of PByte;
  end;

  BrotliMetablockBodyArena = packed record
    dist_extra_bits: array [0 .. 543] of uint8_t;
    dist_offset: array [0 .. 543] of uint32_t;
  end;

  BrotliDecoderStateStruct = packed record
    state: BrotliRunningState;
    loop_counter: longint;
    br: BrotliBitReader;
    alloc_func: tbrotli_alloc_func;
    free_func: tbrotli_free_func;
    memory_manager_opaque: pointer;

    buffer: record
      case longint of
        0:
          (u64: uint64_t);
        1:
          (u8: array [0 .. 7] of uint8_t);
    end;

    buffer_length: uint32_t;
    pos: longint;
    max_backward_distance: longint;
    max_distance: longint;
    ringbuffer_size: longint;
    ringbuffer_mask: longint;
    dist_rb_idx: longint;
    dist_rb: array [0 .. 3] of longint;
    error_code: longint;
    ringbuffer: PByte;
    ringbuffer_end: PByte;
    htree_command: PHuffmanCode;
    context_lookup: ^uint8_t;
    context_map_slice: ^uint8_t;
    dist_context_map_slice: ^uint8_t;
    literal_hgroup: HuffmanTreeGroup;
    insert_copy_hgroup: HuffmanTreeGroup;
    distance_hgroup: HuffmanTreeGroup;
    block_type_trees: PHuffmanCode;
    block_len_trees: PHuffmanCode;
    trivial_literal_context: longint;
    distance_context: longint;
    meta_block_remaining_len: longint;
    block_length_index: uint32_t;
    block_length: array [0 .. 2] of uint32_t;
    num_block_types: array [0 .. 2] of uint32_t;
    block_type_rb: array [0 .. 5] of uint32_t;
    distance_postfix_bits: uint32_t;
    num_direct_distance_codes: uint32_t;
    num_dist_htrees: uint32_t;
    dist_context_map: ^uint8_t;
    literal_htree: ^HuffmanCode;
    dist_htree_index: uint8_t;
    copy_length: longint;
    distance_code: longint;
    rb_roundtrips: size_t;
    partial_pos_out: size_t;
    mtf_upper_bound: uint32_t;
    mtf: array [0 .. (64 + 1) - 1] of uint32_t;
    metadata_start_func: tbrotli_decoder_metadata_start_func;
    metadata_chunk_func: tbrotli_decoder_metadata_chunk_func;
    metadata_callback_opaque: pointer;
    used_input: uint64_t;
    substate_metablock_header: BrotliRunningMetablockHeaderState;
    substate_uncompressed: BrotliRunningUncompressedState;
    substate_decode_uint8: BrotliRunningDecodeUint8State;
    substate_read_block_length: BrotliRunningReadBlockLengthState;
    flag0: Word;
    window_bits: uint32_t;
    new_ringbuffer_size: longint;
    num_literal_htrees: uint32_t;
    context_map: ^uint8_t;
    context_modes: ^uint8_t;
    dictionary: PBrotliSharedDictionaryStruct;
    compound_dictionary: ^BrotliDecoderCompoundDictionary;
    trivial_literal_contexts: array [0 .. 7] of uint32_t;

    arena: record
      case longint of
        0:
          (header: BrotliMetablockHeaderArena);
        1:
          (body: BrotliMetablockBodyArena);
    end;
  end;

  PBrotliDecoderState = ^BrotliDecoderStateStruct;

type
  BrotliMemoryManager = packed record
    alloc_func: tbrotli_alloc_func;
    free_func: tbrotli_free_func;
    opaque: pointer;
    is_oom: BROTLI_BOOL;
    perm_allocated: size_t;
    new_allocated: size_t;
    new_freed: size_t;
    pointers: array [0 .. 255] of pointer;
  end;

  PBrotliMemoryManager = ^BrotliMemoryManager;

type
  BrotliEncoderMode = (BROTLI_MODE_GENERIC = 0, BROTLI_MODE_TEXT = 1,
    BROTLI_MODE_FONT = 2);

  BrotliEncoderOperation = (BROTLI_OPERATION_PROCESS = 0,
    BROTLI_OPERATION_FLUSH = 1, BROTLI_OPERATION_FINISH = 2,
    BROTLI_OPERATION_EMIT_METADATA = 3);

  BrotliEncoderParameter = (BROTLI_PARAM_MODE = 0, BROTLI_PARAM_QUALITY = 1,
    BROTLI_PARAM_LGWIN = 2, BROTLI_PARAM_LGBLOCK = 3,
    BROTLI_PARAM_DISABLE_LITERAL_CONTEXT_MODELING = 4,
    BROTLI_PARAM_SIZE_HINT = 5, BROTLI_PARAM_LARGE_WINDOW = 6,
    BROTLI_PARAM_NPOSTFIX = 7, BROTLI_PARAM_NDIRECT = 8,
    BROTLI_PARAM_STREAM_OFFSET = 9);

  BrotliEncoderStreamState = (BROTLI_STREAM_PROCESSING = 0,
    BROTLI_STREAM_FLUSH_REQUESTED = 1, BROTLI_STREAM_FINISHED = 2,
    BROTLI_STREAM_METADATA_HEAD = 3, BROTLI_STREAM_METADATA_BODY = 4);

  BrotliEncoderFlintState = (BROTLI_FLINT_NEEDS_2_BYTES = 2,
    BROTLI_FLINT_NEEDS_1_BYTE = 1, BROTLI_FLINT_WAITING_FOR_PROCESSING = 0,
    BROTLI_FLINT_WAITING_FOR_FLUSHING = -(1), BROTLI_FLINT_DONE = -(2));

  BrotliHasherParams = packed record
  var
    &type, bucket_bits, block_bits, hash_len,
      num_last_distances_to_check: size_t;
  end;

  BrotliCommand = record
  var
    insert_len_, copy_len_, dist_extra_: UInt32;
    cmd_prefix_, dist_prefix_: uint16;
  end;

  PBrotliCommand = ^BrotliCommand;

  BrotliRingBuffer = record
  var
    size_, mask_, tail_size_, total_size_, cur_size_, pos_: UInt32;
    data_, buffer_: PByte;
  end;

  PBrotliRingBuffer = ^BrotliRingBuffer;

  DictWord = packed record
  var
    len, transform: Byte;
    idx: uint16;
  end;

  PDictWord = ^DictWord;

  BrotliTrieNode = packed record
  var
    single, c, len_: Byte;
    idx_, sub: UInt32;
  end;

  PBrotliTrieNode = ^BrotliTrieNode;

  BrotliTrie = packed record
  var
    pool: PBrotliTrieNode;
    pool_capacity, pool_size: size_t;
    root: PBrotliTrieNode;
  end;

  PContextualEncoderDictionary = ^ContextualEncoderDictionary;

  BrotliEncoderDictionary = packed record
  var
    words: BrotliDictionary;
    num_transforms, cutoffTransformsCount: UInt32;
    cutOffTransforms: UInt64;
    hash_table_words: uint16_t;
    hash_table_lengths: uint8_t;
    buckets: uint16_t;
    dict_words: DictWord;
    trie: BrotliTrie;
    has_words_heavy: BROTLI_BOOL;
    parent: PContextualEncoderDictionary;
    hash_table_data_words_: uint16_t;
    hash_table_data_lengths_: uint8_t;
    buckets_alloc_size_: size_t;
    buckets_data_: uint16_t;
    dict_words_alloc_size_: size_t;
    dict_words_data_: PDictWord;
    words_instance_: PBrotliDictionary;
  end;

  PBrotliEncoderDictionary = ^BrotliEncoderDictionary;

  ContextualEncoderDictionary = packed record
  var
    context_based: BROTLI_BOOL;
    num_dictionaries: Byte;
    context_map: array [0 .. 63] of Byte;
    dict: array [0 .. 63] of BrotliEncoderDictionary;
    num_instances_: size_t;
    instance_: BrotliEncoderDictionary;
    instances_: PBrotliEncoderDictionary;
  end;

type
  PreparedDictionary = packed record
  var
    magic, num_items, source_size, hash_bits, bucket_bits, slot_bits: UInt32;
  end;

  PPreparedDictionary = ^PreparedDictionary;

  CompoundDictionary = packed record
  var
    num_chunks, total_size: size_t;
    chunks: array [0 .. 15] of PPreparedDictionary;
    chunk_source: array [0 .. 15] of PByte;
    chunk_offsets: array [0 .. 15] of size_t;
    num_prepared_instances_: size_t;
    prepared_instances_: array [0 .. 15] of PreparedDictionary;
  end;

type
  SharedEncoderDictionary = packed record
  var
    magic: UInt32;
    compound: CompoundDictionary;
    contextual: ContextualEncoderDictionary;
    max_quality: integer;
  end;

  BrotliDistanceParams = packed record
  var
    distance_postfix_bits, num_direct_distance_codes, alphabet_size_max,
      alphabet_size_limit: UInt32;
    max_distance: size_t;
  end;

  BrotliEncoderParams = packed record
  var
    mode: BrotliEncoderMode;
    quality, lgwin, lgblock: size_t;
    stream_offset, size_hint: size_t;
    disable_literal_context_modeling, large_window: BROTLI_BOOL;
    hasher: BrotliHasherParams;
    dist: BrotliDistanceParams;
    dictionary: SharedEncoderDictionary;
  end;

  BrotliEncoderStateStruct = packed record
  var
    params: BrotliEncoderParams;
    memory_manager_: PBrotliMemoryManager;
    input_pos_: UInt64;
    ringbuffer_: BrotliRingBuffer;
    cmd_alloc_size_: size_t;
    commands_: PBrotliCommand;
    num_commands_, num_literals_, last_insert_len_: size_t;
    last_flush_pos_, last_processed_pos_: UInt64;
    dist_cache_: array [0 .. 15] of integer;
    saved_dist_cache_: array [0 .. 3] of integer;
    last_bytes_: uint16;
    last_bytes_bits_: Byte;
    flint_: int8_t;
    prev_byte_, prev_byte2_: Byte;
    storage_size_: size_t;
    storage_: uint8_t;
    hasher_: pointer;
    small_table_: array [0 .. (1 shl 10) - 1] of size_t;
    large_table_: size_t;
    large_table_size_: size_t;
    one_pass_arena_: pointer;
    two_pass_arena_: pointer;
    command_buf_: uint32_t;
    literal_buf_: uint8_t;
    total_in_: UInt64;
    next_out_: uint8_t;
    available_out_: size_t;
    total_out_: UInt64;

    tiny_buf_: record
      case longint of
        0:
          (u64: array [0 .. 1] of UInt64);
        1:
          (u8: array [0 .. 15] of Byte);
    end;

    remaining_metadata_bytes_: UInt32;
    stream_state_: BrotliEncoderStreamState;
    is_last_block_emitted_, is_initialized_: BROTLI_BOOL;
  end;

  PBrotliEncoderState = ^BrotliEncoderStateStruct;

  { === Version helpers === }

type
  TBrotliVersion = record
    Major: UInt32;
    Minor: UInt32;
    Patch: UInt32;
  end;

function BrotliDecoderVersionValue: UInt32; inline;
function BrotliEncoderVersionValue: UInt32; inline;

function BrotliVersionDecode(V: UInt32): TBrotliVersion; inline;


  { Common }

function BrotliDefaultAllocFunc(opaque: pointer; size: size_t): pointer;
  cdecl; external;

procedure BrotliDefaultFreeFunc(opaque: pointer; address: pointer);
  cdecl; external;
{ Decoder }
procedure BrotliInitBitReader(br: pointer); cdecl; external;

function BrotliWarmupBitReader(br: pointer): BROTLI_BOOL; cdecl; external;

function BrotliGetTransforms(): pointer; cdecl; external;

function BrotliTransformDictionaryWord(dts: PByte; w: PByte; len: uint32_t;
  const transforms: pointer; transform_idx: uint32_t): uint32_t; cdecl;
  external;

function BrotliGetDictionary(): pointer; cdecl; external;
procedure BrotliSetDictionaryData(const data: pointer); cdecl; external;

function BrotliDecoderSetParameter(state: pointer;
  param: BrotliDecoderParameter; value: uint32_t): longint; cdecl; external;

function BrotliDecoderCreateInstance(alloc_func: tbrotli_alloc_func;
  free_func: tbrotli_free_func; opaque: pointer): pointer; cdecl; external;

procedure BrotliDecoderDestroyInstance(state: pointer); cdecl; external;

function BrotliDecoderDecompress(encoded_size: size_t; encoded_buffer: pointer;
  decoded_size: Psize_t; decoded_buffer: pointer): BrotliDecoderResult;
  cdecl; external;

function BrotliDecoderDecompressStream(state: pointer; available_in: Psize_t;
  next_in: ppbyte; available_out: Psize_t; next_out: ppbyte; total_out: Psize_t)
  : BrotliDecoderResult; cdecl; external;

function BrotliDecoderHasMoreOutput(state: pointer): longint; cdecl; external;

function BrotliDecoderTakeOutput(state: pointer; size: Psize_t): PByte;
  cdecl; external;

function BrotliDecoderIsUsed(state: pointer): longint; cdecl; external;

function BrotliDecoderIsFinished(state: pointer): longint; cdecl; external;

function BrotliDecoderGetErrorCode(state: pointer): BrotliDecoderErrorCode;
  cdecl; external;

function BrotliDecoderErrorString(c: BrotliDecoderErrorCode): pansichar;
  cdecl; external;

function BrotliDecoderVersion: uint32_t; cdecl; external;

{ ENCODER }

procedure BrotliPopulationCostCommand; cdecl; external;
procedure BrotliPopulationCostDistance; cdecl; external;
procedure BrotliPopulationCostLiteral; cdecl; external;
procedure _kBrotliContextLookupTable; cdecl; external;
procedure _kBrotliPrefixCodeRanges; cdecl; external;
procedure kBrotliBitMask; cdecl; external;
procedure kStaticDictionaryHashWords; cdecl; external;
procedure kStaticDictionaryHashLengths; cdecl; external;
procedure BrotliBuildAndStoreHuffmanTreeFast; cdecl; external;
procedure BrotliStoreHuffmanTree; cdecl; external;
procedure BrotliCompressFragmentTwoPass; cdecl; external;
procedure BrotliCompressFragmentFast; cdecl; external;
procedure BrotliCreateZopfliBackwardReferences; cdecl; external;
procedure BrotliCreateHqZopfliBackwardReferences; cdecl; external;
procedure BrotliCreateBackwardReferences; cdecl; external;
procedure BrotliStoreUncompressedMetaBlock; cdecl; external;
procedure BrotliStoreMetaBlockFast; cdecl; external;
procedure BrotliStoreMetaBlockTrivial; cdecl; external;
procedure BrotliInitBlockSplit; cdecl; external;
procedure BrotliStoreMetaBlock; cdecl; external;
procedure BrotliDestroyBlockSplit; cdecl; external;
procedure BrotliSplitBlock; cdecl; external;
procedure BrotliBuildHistogramsWithContext; cdecl; external;
procedure BrotliClusterHistogramsLiteral; cdecl; external;
procedure BrotliClusterHistogramsDistance; cdecl; external;
procedure BrotliOptimizeHuffmanCountsForRle; cdecl; external;

function BrotliEncoderSetParameter(state: pointer;
  param: BrotliEncoderParameter; value: uint32_t): longint; cdecl; external;

function BrotliEncoderCreateInstance(alloc_func: tbrotli_alloc_func;
  free_func: tbrotli_free_func; opaque: pointer): pointer; cdecl; external;

procedure BrotliEncoderDestroyInstance(state: pointer); cdecl; external;

function BrotliEncoderMaxCompressedSize(input_size: size_t): size_t;
  cdecl; external;

function BrotliEncoderCompress(quality: integer; lgwin: integer;
  mode: BrotliEncoderMode; input_size: size_t; input_buffer: pointer;
  encoded_size: Psize_t; encoded_buffer: pointer): longint; cdecl; external;

function BrotliEncoderCompressStream(state: pointer; op: BrotliEncoderOperation;
  available_in: Psize_t; next_in: ppbyte; available_out: Psize_t;
  next_out: ppbyte; total_out: Psize_t): BROTLI_BOOL; cdecl; external;

function BrotliEncoderIsFinished(state: pointer): BROTLI_BOOL; cdecl; external;

function BrotliEncoderHasMoreOutput(state: pointer): BROTLI_BOOL;
  cdecl; external;

function BrotliEncoderTakeOutput(state: pointer; size: Psize_t): PByte;
  cdecl; external;
procedure BrotliInitZopfliNodes; cdecl; external;
procedure BrotliZopfliComputeShortestPath; cdecl; external;
procedure BrotliZopfliCreateCommands; cdecl; external;

function BrotliEncoderVersion: uint32_t; cdecl; external;

implementation

{$IFNDEF FPC}
{$IFDEF MSWINDOWS}

uses
  System.Win.Crtl; {$NOINCLUDE System.Win.Crtl}
{$ENDIF}
{$ENDIF}
{$IFNDEF FPC}
{$IFDEF WIN64}
{ common }
{$L objects/win64/common/constants.obj}
{$L objects/win64/common/context.obj}
{$L objects/win64/common/dictionary.obj}
{$L objects/win64/common/platform.obj}
{$L objects/win64/common/transform.obj}
{ decoder }
{$L objects/win64/decode/bit_reader.obj}
{$L objects/win64/decode/decode.obj}
{$L objects/win64/decode/huffman.obj}
{$L objects/win64/decode/state.obj}
{ encoder }
{$L objects/win64/encode/backward_references.obj}
{$L objects/win64/encode/backward_references_hq.obj}
{$L objects/win64/encode/bit_cost.obj}
{$L objects/win64/encode/block_splitter.obj}
{$L objects/win64/encode/brotli_bit_stream.obj}
{$L objects/win64/encode/cluster.obj}
{$L objects/win64/encode/command.obj}
{$L objects/win64/encode/compress_fragment.obj}
{$L objects/win64/encode/compress_fragment_two_pass.obj}
{$L objects/win64/encode/dictionary_hash.obj}
{$L objects/win64/encode/encode.obj}
{$L objects/win64/encode/encoder_dict.obj}
{$L objects/win64/encode/entropy_encode.obj}
{$L objects/win64/encode/fast_log.obj}
{$L objects/win64/encode/histogram.obj}
{$L objects/win64/encode/literal_cost.obj}
{$L objects/win64/encode/memory.obj}
{$L objects/win64/encode/metablock.obj}
{$L objects/win64/encode/static_dict.obj}
{$L objects/win64/encode/utf8_util.obj}
{$ENDIF}
{$ELSE}
{$IFDEF WIN64}
  {$linklib objects/fpc-win64/decode/libbrotlidec.a}
  {$linklib objects/fpc-win64/encode/libbrotlienc.a}
  {$linklib objects/fpc-win64/common/libbrotlicommon.a}
{$ENDIF}
{$IFDEF Linux}
  {$linklib objects/fpc-lin64/decode/libbrotlidec.a}
  {$linklib objects/fpc-lin64/encode/libbrotlienc.a}
  {$linklib objects/fpc-lin64/common/libbrotlicommon.a}
{$ENDIF}
{$ENDIF}
{$IFDEF MSWINDOWS}
function malloc(size: size_t): pointer; cdecl; export;
begin
  GetMem(Result, size);
end;

procedure free(Ptr: pointer); cdecl; export;
begin
  FreeMem(Ptr);
end;

function calloc(count, size: size_t): pointer; cdecl; export;
var
  n: NativeUInt;
begin
  n := count * size;
  GetMem(Result, n);
  FillChar(Result^, n, 0);
end;

function realloc(Ptr: pointer; size: size_t): pointer; cdecl; export;
begin
  if Ptr = nil then
  begin
    GetMem(Result, size);
    Exit;
  end;

  if size = 0 then
  begin
    FreeMem(Ptr);
    Result := nil;
    Exit;
  end;

  ReallocMem(Ptr, size);
  Result := Ptr;
end;

function memset(str: pointer; c: integer; n: size_t): pointer; cdecl; export;
begin
  FillChar(str^, n, c);
end;

function memmove(dest: pointer; const src: pointer; count: size_t): pointer;
  cdecl; export;
begin
  Move(src^, dest^, count);
  Result := dest;
end;

function memcpy(dest: pointer; const src: pointer; count: size_t): pointer;
  cdecl; export;
begin
  Move(src^, dest^, count);
  Result := dest;
end;
{$ENDIF}

function log2(X: Double): Double; cdecl; export;
begin
  Result := Ln(X) / Ln(2.0);
end;

{$IFDEF FPC}
{$ASMMODE intel}
{$ENDIF}
{$IFDEF FPC}
{$IFDEF WIN64}
procedure ___chkstk_ms; assembler; nostackframe; public name '___chkstk_ms';
asm
  push   rcx
  push   rax

  cmp    rax, $1000
  jb     @done

  lea    rcx, [rsp + $18]

@loop:
  sub    rcx, $1000
  test   byte ptr [rcx], 0
  sub    rax, $1000
  cmp    rax, $1000
  ja     @loop

@done:
  sub    rcx, rax
  test   byte ptr [rcx], 0  // last page

  pop    rax
  pop    rcx
  ret
end;
{$ENDIF}
{$ELSE}

procedure __chkstk; assembler;
asm
  PUSH   RCX
  PUSH   RAX

  CMP    RAX, $1000  // 4096 byte (page size)
  LEA    RCX, [RSP + 24]
  JB     @@Done

@@Loop:
  SUB    RCX, $1000
  OR     [RCX], RCX
  SUB    RAX, $1000
  CMP    RAX, $1000
  JA     @@Loop

@@Done:
  SUB    RCX, RAX
  OR     [RCX], RCX

  POP    RAX
  POP    RCX
  RET
end;
{$ENDIF}

function _Log(X: Double): Double; cdecl;
begin
  Result := Ln(X);
end;

function _Inf: Double; cdecl;
const
  PosInfinity: UInt64 = $7FF0000000000000; // IEEE 754 +Infinity
begin
  Result := PDouble(@PosInfinity)^;
end;

procedure exit(const Status: integer); cdecl; export;
begin

end;

function TO_BROTLI_BOOL(X: longint): longint;
begin
  if X = 0 then
    Result := BROTLI_FALSE
  else
    Result := BROTLI_TRUE;
end;

function BROTLI_MAKE_UINT64_T(high, low: longint): longint;
begin
  BROTLI_MAKE_UINT64_T := ((uint64_t(high)) shl 32) or low;
end;

function BROTLI_UINT32_MAX: longint;
begin
  BROTLI_UINT32_MAX := not(uint32_t(0));
end;

function BROTLI_SIZE_MAX: longint;
begin
  BROTLI_SIZE_MAX := not(size_t(0));
end;

function BrotliDecoderVersionValue: UInt32; inline;
begin
  Result := BrotliDecoderVersion;
end;

function BrotliEncoderVersionValue: UInt32; inline;
begin
  Result := BrotliEncoderVersion;
end;

// brotli version packed as: (major << 24) | (minor << 12) | patch
function BrotliVersionDecode(V: UInt32): TBrotliVersion; inline;
begin
  Result.Major := (V shr 24) and $FF;
  Result.Minor := (V shr 12) and $FFF;
  Result.Patch := V and $FFF;
end;


end.




