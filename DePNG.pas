{$INCLUDE switches}

unit DePNG;

{ Load TBitmap from PNG file
  Reduced version of the PngImage unit by Edmund H. Hand
  Modifications by Steffen Gerlach <mail@steffengerlach.de>

  PngImage.Pas - Copyright 1998 Edmund H. Hand
  For conditions of distribution and use, see the COPYRIGHT NOTICE below.

  COPYRIGHT NOTICE:

  The unit is supplied "AS IS".  The Author disclaims all warranties,
  expressed or implied, including, without limitation, the warranties of
  merchantability and of fitness for any purpose.  The Author assumes no
  liability for direct, indirect, incidental, special, exemplary, or
  consequential damages, which may result from the use of this unit, even
  if advised of the possibility of such damage.

  Permission is hereby granted to use, copy, modify, and distribute this
  source code, or portions hereof, for any purpose, without fee, subject
  to the following restrictions:
  1. The origin of this source code must not be misrepresented.
  2. Altered versions must be plainly marked as such and must not be
     misrepresented as being the original source.
  3. This Copyright notice may not be removed or altered from any source or
     altered source distribution.

  If you use this source code in a product, acknowledgment is not required
  but would be appreciated.  I would also like to know of any projects,
  especially commercial ones, that use this code.
}

interface

uses Graphics;

function LoadBitmapFromPNG(var self: TBitmap; const FileName: string): boolean;


implementation

uses SysUtils, Windows;

{ Reduced version of the PngLib unit by Edmund H. Hand
  Modifications by Steffen Gerlach <mail@steffengerlach.de>

  Conversion from C header file to Pascal Unit by Edmund H. Hand in
  April of 1998.
  For conditions of distribution and use, see the COPYRIGHT NOTICE from the
  orginal C Header file below.

  This unit is intended for use with LPng.DLL.  LPng.DLL was compiled with
  Microsoft Visual C++ 5.0 and is comprised of the standard PNG library
  version 1.0.1.  LPng.DLL uses ZLib 1.1.2 internally for compression and
  decompression.  The ZLib functions are also exported in the DLL, but they
  have not yet been defined here.

  The primary word of warning I must offer is that most of the function
  pointers for callback functions have merely been declared as the pascal
  Pointer type. I have only defined one procedure type for read and write
  callback functions.  So if you plan to use other callback types, check the
  included header file for the function definition.

  The header comments of the original C Header file follow.

 * png.h - header file for PNG reference library
 *
 * libpng 1.0.1
 * For conditions of distribution and use, see the COPYRIGHT NOTICE below.
 * Copyright (c) 1995, 1996 Guy Eric Schalnat, Group 42, Inc.
 * Copyright (c) 1996, 1997 Andreas Dilger
 * Copyright (c) 1998 Glenn Randers-Pehrson
 * March 15, 1998
 *
 * Note about libpng version numbers:
 *
 *    Due to various miscommunications, unforeseen code incompatibilities
 *    and occasional factors outside the authors' control, version numbering
 *    on the library has not always been consistent and straightforward.
 *    The following table summarizes matters since version 0.89c, which was
 *    the first widely used release:
 *
 *      source                    png.h   png.h   shared-lib
 *      version                   string    int   version
 *      -------                   ------  ------  ----------
 *      0.89c ("1.0 beta 3")      0.89        89  1.0.89
 *      0.90  ("1.0 beta 4")      0.90        90  0.90  [should have been 2.0.90]
 *      0.95  ("1.0 beta 5")      0.95        95  0.95  [should have been 2.0.95]
 *      0.96  ("1.0 beta 6")      0.96        96  0.96  [should have been 2.0.96]
 *      0.97b ("1.00.97 beta 7")  1.00.97     97  1.0.1 [should have been 2.0.97]
 *      0.97c                     0.97        97  2.0.97
 *      0.98                      0.98        98  2.0.98
 *      0.99                      0.99        98  2.0.99
 *      0.99a-m                   0.99        99  2.0.99
 *      1.00                      1.00       100  2.1.0 [int should be 10000]
 *      1.0.0                     1.0.0      100  2.1.0 [int should be 10000]
 *      1.0.1                     1.0.1    10001  2.1.0
 *
 *    Henceforth the source version will match the shared-library minor
 *    and patch numbers; the shared-library major version number will be
 *    used for changes in backward compatibility, as it is intended.
 *    The PNG_PNGLIB_VER macro, which is not used within libpng but
 *    is available for applications, is an unsigned integer of the form
 *    xyyzz corresponding to the source version x.y.z (leading zeros in y and z).
 *    
 *
 * See libpng.txt for more information.  The PNG specification is available
 * as RFC 2083 <ftp://ftp.uu.net/graphics/png/documents/>
 * and as a W3C Recommendation <http://www.w3.org/TR/REC.png.html>
 *
 * Contributing Authors:
 *    John Bowler
 *    Kevin Bracey
 *    Sam Bushell
 *    Andreas Dilger
 *    Magnus Holmgren
 *    Tom Lane
 *    Dave Martindale
 *    Glenn Randers-Pehrson
 *    Greg Roelofs
 *    Guy Eric Schalnat
 *    Paul Schmidt
 *    Tom Tanner
 *    Willem van Schaik
 *    Tim Wegner
 *
 * The contributing authors would like to thank all those who helped
 * with testing, bug fixes, and patience.  This wouldn't have been
 * possible without all of you.
 *
 * Thanks to Frank J. T. Wojcik for helping with the documentation.
 *
 * COPYRIGHT NOTICE:
 *
 * The PNG Reference Library is supplied "AS IS".  The Contributing Authors
 * and Group 42, Inc. disclaim all warranties, expressed or implied,
 * including, without limitation, the warranties of merchantability and of
 * fitness for any purpose.  The Contributing Authors and Group 42, Inc.
 * assume no liability for direct, indirect, incidental, special, exemplary,
 * or consequential damages, which may result from the use of the PNG
 * Reference Library, even if advised of the possibility of such damage.
 *
 * Permission is hereby granted to use, copy, modify, and distribute this
 * source code, or portions hereof, for any purpose, without fee, subject
 * to the following restrictions:
 * 1. The origin of this source code must not be misrepresented.
 * 2. Altered versions must be plainly marked as such and must not be
 *    misrepresented as being the original source.
 * 3. This Copyright notice may not be removed or altered from any source or
 *    altered source distribution.
 *
 * The Contributing Authors and Group 42, Inc. specifically permit, without
 * fee, and encourage the use of this source code as a component to
 * supporting the PNG file format in commercial products.  If you use this
 * source code in a product, acknowledgment is not required but would be
 * appreciated.
 *}

type PByte      = ^Byte;
type PPByte     = ^PByte;
type PPChar     = ^PChar;
type PWord      = ^Word;
type PPWord     = ^PWord;
type PDouble    = ^Double;
type PSmallint  = ^Smallint;
type PCardinal  = ^Cardinal;
type PPCardinal = ^PCardinal;
type PInteger   = ^Integer;
type PPInteger  = ^PInteger;

const Lib = 'lpng.dll';

const
PNG_LIBPNG_VER_STRING =  '1.0.1';

PNG_COLOR_TYPE_GRAY:       Integer = 0;
PNG_COLOR_TYPE_PALETTE:    Integer = 3;

PNG_INFO_tRNS: Cardinal = $0010;


{* Three color definitions.  The order of the red, green, and blue, (and the
 * exact size) is not important, although the size of the fields need to
 * be png_byte or png_uint_16 (as defined below).
 *}
type TPng_Color = record
  red:   Byte;
  green: Byte;
  blue:  Byte;
end;
type PPng_Color  = ^TPng_Color;
type PPPng_Color = ^PPng_Color;

type TPng_Color_16 = record
  index: Byte;     // Used for palette files
  red:   Word;     // For use in reg, green, blue files
  green: Word;
  blue:  Word;
  gray:  Word;     // For use in grayscale files
end;
type PPng_Color_16  = ^TPng_Color_16;
type PPPng_Color_16 = ^PPng_Color_16;

type TPng_Color_8 = record
   red:   Byte;    // for use in red green blue files
   green: Byte;
   blue:  Byte;
   gray:  Byte;    // for use in grayscale files
   alpha: Byte;    // for alpha channel files
end;
type PPng_Color_8  = ^TPng_Color_8;
type PPPng_Color_8 = ^PPng_Color_8;

{* png_text holds the text in a PNG file, and whether they are compressed
 * in the PNG file or not.  The "text" field points to a regular C string.
 *}
type TPng_Text = record
   compression: Integer;   // compression value, see PNG_TEXT_COMPRESSION_
   key:         PChar;     // keyword, 1-79 character description of "text"
   text:        PChar;     // comment, may be an empty string (ie "")
   text_length: Integer;   // length of "text" field
end;
type PPng_Text  = ^TPng_Text;
type PPPng_Text = ^PPng_Text;
type TPng_Text_Array = array[0..65535] of TPng_Text;
type PPng_Text_Array = ^TPng_Text_Array;

{* png_time is a way to hold the time in an machine independent way.
 * Two conversions are provided, both from time_t and struct tm.  There
 * is no portable way to convert to either of these structures, as far
 * as I know.  If you know of a portable way, send it to me.  As a side
 * note - PNG is Year 2000 compliant!
 *}
type TPng_Time = record
   year:   Word; // full year, as in, 1995
   month:  Byte; // month of year, 1 - 12
   day:    Byte; // day of month, 1 - 31
   hour:   Byte; // hour of day, 0 - 23
   minute: Byte; // minute of hour, 0 - 59
   second: Byte; // second of minute, 0 - 60 (for leap seconds)
end;
type PPng_Time  = ^TPng_Time;
type PPPng_Time = ^PPng_Time;

type TPng_Info = record
   //the following are necessary for every PNG file
   width:            Cardinal;  // width of image in pixels (from IHDR)
   height:           Cardinal;  // height of image in pixels (from IHDR)
   valid:            Cardinal;  // valid chunk data (see PNG_INFO_ below)
   rowbytes:         Cardinal;  // bytes needed to hold an untransformed row
   palette:          PPng_Color;// array of color values (valid & PNG_INFO_PLTE)
   num_palette:      Word;      // number of color entries in "palette" (PLTE)
   num_trans:        Word;      // number of transparent palette color (tRNS)
   bit_depth:        Byte;      // 1, 2, 4, 8, or 16 bits/channel (from IHDR)
   color_type:       Byte;      // see PNG_COLOR_TYPE_ below (from IHDR)
   compression_type: Byte;      // must be PNG_COMPRESSION_TYPE_BASE (IHDR)
   filter_type:      Byte;      // must be PNG_FILTER_TYPE_BASE (from IHDR)
   interlace_type:   Byte;      // One of PNG_INTERLACE_NONE,PNG_INTERLACE_ADAM7

   // The following is informational only on read, and not used on writes.
   channels:     Byte;    // number of data channels per pixel (1, 3, 4)
   pixel_depth:  Byte;    // number of bits per pixel
   spare_byte:   Byte;    // to align the data, and for future use
   signature: array[0..7] of Byte;// magic bytes read by libpng from start of file

   {* The rest of the data is optional.  If you are reading, check the
    * valid field to see if the information in these are valid.  If you
    * are writing, set the valid field to those chunks you want written,
    * and initialize the appropriate fields below.
    *}

   {* The gAMA chunk describes the gamma characteristics of the system
    * on which the image was created, normally in the range [1.0, 2.5].
    * Data is valid if (valid & PNG_INFO_gAMA) is non-zero.
    *}
   gamma: Single;  // gamma value of image, if (valid & PNG_INFO_gAMA)

    // GR-P, 0.96a
    // Data valid if (valid & PNG_INFO_sRGB) non-zero.
   srgb_intent: Byte;       // sRGB rendering intent [0, 1, 2, or 3]

   {* The tEXt and zTXt chunks contain human-readable textual data in
    * uncompressed and compressed forms, respectively.  The data in "text"
    * is an array of pointers to uncompressed, null-terminated C strings.
    * Each chunk has a keyword which describes the textual data contained
    * in that chunk.  Keywords are not required to be unique, and the text
    * string may be empty.  Any number of text chunks may be in an image.
    *}
   num_text: Integer;   // number of comments read/to write
   max_text: Integer;   // current size of text array
   text:     PPng_Text; // array of comments read/to write

   {* The tIME chunk holds the last time the displayed image data was
    * modified.  See the png_time struct for the contents of this struct.
    *}
   mod_time: TPng_Time;

   {* The sBIT chunk specifies the number of significant high-order bits
    * in the pixel data.  Values are in the range [1, bit_depth], and are
    * only specified for the channels in the pixel data.  The contents of
    * the low-order bits is not specified.  Data is valid if
    * (valid & PNG_INFO_sBIT) is non-zero.
    *}
   sig_bit: TPng_Color_8;  // significant bits in color channels

   {* The tRNS chunk supplies transparency data for paletted images and
    * other image types that don't need a full alpha channel.  There are
    * "num_trans" transparency values for a paletted image, stored in the
    * same order as the palette colors, starting from index 0.  Values
    * for the data are in the range [0, 255], ranging from fully transparent
    * to fully opaque, respectively.  For non-paletted images, there is a
    * single color specified which should be treated as fully transparent.
    * Data is valid if (valid & PNG_INFO_tRNS) is non-zero.
    *}
   trans: PByte; // transparent values for paletted image
   trans_values: TPng_Color_16; // transparent color for non-palette image

   {* The bKGD chunk gives the suggested image background color if the
    * display program does not have its own background color and the image
    * is needs to composited onto a background before display.  The colors
    * in "background" are normally in the same color space/depth as the
    * pixel data.  Data is valid if (valid & PNG_INFO_bKGD) is non-zero.
    *}
   background: TPng_Color_16;

   {* The oFFs chunk gives the offset in "offset_unit_type" units rightwards
    * and downwards from the top-left corner of the display, page, or other
    * application-specific co-ordinate space.  See the PNG_OFFSET_ defines
    * below for the unit types.  Valid if (valid & PNG_INFO_oFFs) non-zero.
    *}
   x_offset:         Cardinal; // x offset on page
   y_offset:         Cardinal; // y offset on page
   offset_unit_type: Byte;     // offset units type

   {* The pHYs chunk gives the physical pixel density of the image for
    * display or printing in "phys_unit_type" units (see PNG_RESOLUTION_
    * defines below).  Data is valid if (valid & PNG_INFO_pHYs) is non-zero.
    *}
   x_pixels_per_unit: Cardinal;  // horizontal pixel density
   y_pixels_per_unit: Cardinal;  // vertical pixel density
   phys_unit_type:    Byte;      // resolution type (see PNG_RESOLUTION_ below)

   {* The hIST chunk contains the relative frequency or importance of the
    * various palette entries, so that a viewer can intelligently select a
    * reduced-color palette, if required.  Data is an array of "num_palette"
    * values in the range [0,65535]. Data valid if (valid & PNG_INFO_hIST)
    * is non-zero.
    *}
   hist: PWord;

   {* The cHRM chunk describes the CIE color characteristics of the monitor
    * on which the PNG was created.  This data allows the viewer to do gamut
    * mapping of the input image to ensure that the viewer sees the same
    * colors in the image as the creator.  Values are in the range
    * [0.0, 0.8].  Data valid if (valid & PNG_INFO_cHRM) non-zero.
    *}
   x_white: Single;
   y_white: Single;
   x_red:   Single;
   y_red:   Single;
   x_green: Single;
   y_green: Single;
   x_blue:  Single;
   y_blue:  Single;

   {* The pCAL chunk describes a transformation between the stored pixel
    * values and original physcical data values used to create the image.
    * The integer range [0, 2^bit_depth - 1] maps to the floating-point
    * range given by [pcal_X0, pcal_X1], and are further transformed by a
    * (possibly non-linear) transformation function given by "pcal_type"
    * and "pcal_params" into "pcal_units".  Please see the PNG_EQUATION_
    * defines below, and the PNG-Group's Scientific Visualization extension
    * chunks document png-scivis-19970203 for a complete description of the
    * transformations and how they should be implemented, as well as the
    * png-extensions document for a description of the ASCII parameter
    * strings.  Data values are valid if (valid & PNG_INFO_pCAL) non-zero.
    *}
   pcal_purpose: PChar;     // pCAL chunk description string
   pcal_X0:      Integer;   // minimum value
   pcal_X1:      Integer;   // maximum value
   pcal_units:   PChar;     // Latin-1 string giving physical units
   pcal_params:  PPChar;    // ASCII strings containing parameter values
   pcal_type:    Byte;      // equation type (see PNG_EQUATION_ below)
   pcal_nparams: Byte;      // number of parameters given in pcal_params
end;
type PPng_Info = ^TPng_Info;
type PPPng_Info = ^PPng_Info;

{* This is used for the transformation routines, as some of them
 * change these values for the row.  It also should enable using
 * the routines for other purposes.
 *}
type TPng_Row_Info = record
   width:       Cardinal; // width of row
   rowbytes:    Cardinal; // number of bytes in row
   color_type:  Byte;     // color type of row
   bit_depth:   Byte;     // bit depth of row
   channels:    Byte;     // number of channels (1, 2, 3, or 4)
   pixel_depth: Byte;     // bits per pixel (depth * channels)
end;
type PPng_Row_Info = ^TPng_Row_Info;
type PPPng_Row_Info = ^PPng_Row_Info;

{* The structure that holds the information to read and write PNG files.
 * The only people who need to care about what is inside of this are the
 * people who will be modifying the library for their own special needs.
 * It should NOT be accessed directly by an application, except to store
 * the jmp_buf.
 *}
type TPng_Struct = record
   jmpbuf: array[0..10] of Integer; // used in png_error
   error_fn: Pointer;    // function for printing errors and aborting
   warning_fn: Pointer;  // function for printing warnings
   error_ptr: Pointer;       // user supplied struct for error functions
   write_data_fn: Pointer;  // function for writing output data
   read_data_fn: Pointer;   // function for reading input data
   read_user_transform_fn: Pointer; // user read transform
   write_user_transform_fn: Pointer; // user write transform
   io_ptr: Integer;         // ptr to application struct for I/O functions

   mode: Cardinal;          // tells us where we are in the PNG file
   flags: Cardinal;         // flags indicating various things to libpng
   transformations: Cardinal; // which transformations to perform

   zstream: Pointer;          // pointer to decompression structure (below)
   zbuf: PByte;            // buffer for zlib
   zbuf_size: Integer;      // size of zbuf
   zlib_level: Integer;            // holds zlib compression level
   zlib_method: Integer;           // holds zlib compression method
   zlib_window_bits: Integer;      // holds zlib compression window bits
   zlib_mem_level: Integer;        // holds zlib compression memory level
   zlib_strategy: Integer;         // holds zlib compression strategy

   width: Cardinal;         // width of image in pixels
   height: Cardinal;        // height of image in pixels
   num_rows: Cardinal;      // number of rows in current pass
   usr_width: Cardinal;     // width of row at start of write
   rowbytes: Cardinal;      // size of row in bytes
   irowbytes: Cardinal;     // size of current interlaced row in bytes
   iwidth: Cardinal;        // width of current interlaced row in pixels
   row_number: Cardinal;    // current row in interlace pass
   prev_row: PByte;        // buffer to save previous (unfiltered) row
   row_buf: PByte;         // buffer to save current (unfiltered) row
   sub_row: PByte;         // buffer to save "sub" row when filtering
   up_row: PByte;          // buffer to save "up" row when filtering
   avg_row: PByte;         // buffer to save "avg" row when filtering
   paeth_row: PByte;       // buffer to save "Paeth" row when filtering
   row_info: TPng_Row_Info;     // used for transformation routines

   idat_size: Cardinal;     // current IDAT size for read
   crc: Cardinal;           // current chunk CRC value
   palette: PPng_Color;        // palette from the input file
   num_palette: Word;   // number of color entries in palette
   num_trans: Word;     // number of transparency values
   chunk_name: array[0..4] of Byte;   // null-terminated name of current chunk
   compression: Byte;      // file compression type (always 0)
   filter: Byte;           // file filter type (always 0)
   interlaced: Byte;       // PNG_INTERLACE_NONE, PNG_INTERLACE_ADAM7
   pass: Byte;             // current interlace pass (0 - 6)
   do_filter: Byte;        // row filter flags (see PNG_FILTER_ below )
   color_type: Byte;       // color type of file
   bit_depth: Byte;        // bit depth of file
   usr_bit_depth: Byte;    // bit depth of users row
   pixel_depth: Byte;      // number of bits per pixel
   channels: Byte;         // number of channels in file
   usr_channels: Byte;     // channels at start of write
   sig_bytes: Byte;        // magic bytes read/written from start of file

   filler: Byte;           // filler byte for 24->32-bit pixel expansion
   background_gamma_type: Byte;
   background_gamma: Single;
   background: TPng_Color_16;   // background color in screen gamma space
   background_1: TPng_Color_16; // background normalized to gamma 1.0
   output_flush_fn: Pointer;// Function for flushing output
   flush_dist: Cardinal;    // how many rows apart to flush, 0 - no flush
   flush_rows: Cardinal;    // number of rows written since last flush
   gamma_shift: Integer;      // number of "insignificant" bits 16-bit gamma
   gamma: Single;          // file gamma value
   screen_gamma: Single;   // screen gamma value (display_gamma/viewing_gamma
   gamma_table: PByte;     // gamma table for 8 bit depth files
   gamma_from_1: PByte;    // converts from 1.0 to screen
   gamma_to_1: PByte;      // converts from file to 1.0
   gamma_16_table: PPWord; // gamma table for 16 bit depth files
   gamma_16_from_1: PPWord; // converts from 1.0 to screen
   gamma_16_to_1: PPWord; // converts from file to 1.0
   sig_bit: TPng_Color_8;       // significant bits in each available channel
   shift: TPng_Color_8;         // shift for significant bit tranformation
   trans: PByte;           // transparency values for paletted files
   trans_values: TPng_Color_16; // transparency values for non-paletted files
   read_row_fn: Pointer;   // called after each row is decoded
   write_row_fn: Pointer; // called after each row is encoded
   info_fn: Pointer; // called after header data fully read
   row_fn: Pointer;   // called after each prog. row is decoded
   end_fn: Pointer;   // called after image is complete
   save_buffer_ptr: PByte;        // current location in save_buffer
   save_buffer: PByte;            // buffer for previously read data
   current_buffer_ptr: PByte;     // current location in current_buffer
   current_buffer: PByte;         // buffer for recently used data
   push_length: Cardinal;          // size of current input chunk
   skip_length: Cardinal;          // bytes to skip in input data
   save_buffer_size: Integer;      // amount of data now in save_buffer
   save_buffer_max: Integer;       // total size of save_buffer
   buffer_size: Integer;           // total amount of available input data
   current_buffer_size: Integer;   // amount of data now in current_buffer
   process_mode: Integer;                 // what push library is currently doing
   cur_palette: Integer;                  // current push library palette index
   current_text_size: Integer;     // current size of text input data
   current_text_left: Integer;     // how much text left to read in input
   current_text: PByte;           // current text chunk buffer
   current_text_ptr: PByte;       // current location in current_text
   palette_lookup: PByte;         // lookup table for dithering
   dither_index: PByte;           // index translation for palette files
   hist: PWord;                // histogram
   heuristic_method: Byte;        // heuristic for row filter selection
   num_prev_filters: Byte;        // number of weights for previous rows
   prev_filters: PByte;           // filter type(s) of previous row(s)
   filter_weights: PWord;      // weight(s) for previous line(s)
   inv_filter_weights: PWord;  // 1/weight(s) for previous line(s)
   filter_costs: PWord;        // relative filter calculation cost
   inv_filter_costs: PWord;    // 1/relative filter calculation cost
   time_buffer: PByte;            // String to hold RFC 1123 time text
end;
type PPng_Struct = ^TPng_Struct;
type PPPng_Struct = ^PPng_Struct;

type
png_set_fnc = procedure (png_ptr: PPng_Struct); cdecl;

png_close_file_fnc = function (filep: Pointer): Integer; cdecl;

png_open_file_fnc = function (fname, mode: PChar): Pointer; cdecl;

png_create_info_struct_fnc = function (png_ptr: PPng_Struct): PPng_Info; cdecl;

png_init_io_fnc = procedure (png_ptr: PPng_Struct; fp: Pointer); cdecl;

png_read_image_fnc = procedure (png_ptr: PPng_Struct; image: PPByte); cdecl;

png_read_info_fnc = procedure (png_ptr: PPng_Struct; info_ptr: PPng_Info);
  cdecl;

png_set_read_status_fn_fnc = procedure (png_ptr: PPng_Struct; read_row_fn:
  Pointer); cdecl;

png_get_rowbytes_fnc = function (png_ptr: PPng_Struct; info_ptr: PPng_Info):
  Cardinal; cdecl;

png_get_valid_fnc = function (png_ptr: PPng_Struct; info_ptr: PPng_Info; flag:
  Cardinal): Cardinal; cdecl;

png_destroy_read_struct_fnc = procedure (png_ptr_ptr: PPPng_Struct;
  info_ptr_ptr, end_info_ptr_ptr: PPPng_Info); cdecl;

png_create_read_struct_fnc = function (user_png_ver: PChar; error_ptr,
  error_fn, warn_fn: Pointer): PPng_Struct; cdecl;

png_get_IHDR_fnc = function (png_ptr: PPng_Struct; info_ptr: PPng_Info; width,
  height: PCardinal; bit_depth, color_type, interlace_type, compression_type,
  filter_type: PInteger): Cardinal; cdecl;

var
png_set_expand, png_set_invert_alpha, png_set_swap, png_set_packing:
  png_set_fnc;
png_close_file: png_close_file_fnc;
png_open_file: png_open_file_fnc;
png_create_info_struct: png_create_info_struct_fnc;
png_init_io: png_init_io_fnc;
png_read_image: png_read_image_fnc;
png_read_info, png_read_update_info: png_read_info_fnc;
png_set_read_status_fn: png_set_read_status_fn_fnc;
png_get_rowbytes: png_get_rowbytes_fnc;
png_get_valid: png_get_valid_fnc;
png_destroy_read_struct: png_destroy_read_struct_fnc;
png_create_read_struct: png_create_read_struct_fnc;
png_get_IHDR: png_get_IHDR_fnc;

HLib: integer;

function LoadBitMapFromPNG;
var
FBitDepth:      Integer;
FBytesPerPixel: Integer;
FColorType:     Integer;
FHeight:        Integer;
FWidth:         Integer;
Data:           PByte;
RowPtrs:        PByte;

  procedure Load;
  var
    cvaluep:  PCardinal;
    y:        Integer;
    png:      PPng_Struct;
    pnginfo:  PPng_Info;
    rowbytes: Cardinal;
    PngFile:  Pointer;
    tmp:      array[0..32] of char;
  begin
    pngfile := png_open_file(@Filename[1], 'rb');
    if pngfile = nil then
      raise Exception.Create('Error Opening File ' + Filename + '!');

    try
      StrPCopy(tmp, PNG_LIBPNG_VER_STRING);
      try
        png := png_create_read_struct(tmp, nil, nil, nil);
        if png <> nil then
        begin
          try
            pnginfo := png_create_info_struct(png);
            png_init_io(png, pngfile);
            png_set_read_status_fn(png, nil);
            png_read_info(png, pnginfo);
            png_get_IHDR(png, pnginfo, @FWidth, @FHeight,
                         @FBitDepth, @FColorType, nil, nil, nil);
            png_set_invert_alpha(png);
            // if bit depth is less than 8 then expand...
            if (FColorType = PNG_COLOR_TYPE_PALETTE) and
               (FBitDepth <= 8) then
              png_set_expand(png);
            if (FColorType = PNG_COLOR_TYPE_GRAY) and
               (FBitDepth < 8) then
              png_set_expand(png);
            // Add alpha channel if pressent
            if png_get_valid(png, pnginfo, PNG_INFO_tRNS) = PNG_INFO_tRNS then
              png_set_expand(png);
            // expand images to 1 pixel per byte
            if FBitDepth < 8 then
              png_set_packing(png);
            // Swap 16 bit images to PC Format
            if FBitDepth = 16 then
              png_set_swap(png);
            // update the info structure
            png_read_update_info(png, pnginfo);
            png_get_IHDR(png, pnginfo, @FWidth, @FHeight,
                         @FBitDepth, @FColorType, nil, nil, nil);

            rowbytes := png_get_rowbytes(png, pnginfo);
            FBytesPerPixel := integer(rowbytes) div FWidth;

            // Initialize Data and RowPtrs
            GetMem(Data, FHeight * FWidth * FBytesPerPixel);
            GetMem(RowPtrs, sizeof(Pointer) * FHeight);
            if (Data <> nil) and (RowPtrs <> nil) then
            begin
              cvaluep := Pointer(RowPtrs);
              for y := 0 to FHeight - 1 do
              begin
                cvaluep^ := Cardinal(Data) + Cardinal(FWidth * FBytesPerPixel * y);
                Inc(cvaluep);
              end;
              // Read the image
              png_read_image(png, PPByte(RowPtrs));
            end;

          finally
            png_destroy_read_struct(@png, @pnginfo, nil);
          end;  // try pnginfo create
        end;  // png <> nil
      except
        raise Exception.Create('Error Reading File!');
      end;  // try png create

    finally
      png_close_file(pngfile);
    end;
  end;  // Load

  procedure Draw;
  var
    valuep:  PByte;
    x, y:    Integer;
    ndx:     Integer;
    sl:      PByteArray;  // Scanline of bitmap
    slbpp:   Integer;     // Scanline bytes per pixel
  begin
    self.Height := FHeight;
    self.Width  := FWidth;
    case FBytesPerPixel of
      2: begin
        self.PixelFormat := pf16Bit;
        slbpp := 2;
      end;
      else begin
        self.PixelFormat := pf24Bit;
        slbpp := 3;
      end;
    end;

    // point to data
    valuep := Data;
    for y := 0 to FHeight - 1 do
    begin
      sl := self.Scanline[y];  // current scanline
      for x := 0 to FWidth - 1 do
      begin
        ndx := x * slbpp;    // index into current scanline
        if FBytesPerPixel = 2 then
        begin
          // handle 16bit grayscale images, this will display them
          // as a 16bit color image, kinda hokie but fits my needs
          // without altering the data.
          sl[ndx]     := valuep^;  Inc(valuep);
          sl[ndx + 1] := valuep^;  Inc(valuep);
        end
        else
        begin
          // RGB - swap blue and red for windows format
          sl[ndx + 2] := valuep^;  Inc(valuep);
          sl[ndx + 1] := valuep^;  Inc(valuep);
          sl[ndx]     := valuep^;  Inc(valuep);
          if FBytesPerPixel = 4 then Inc(valuep); // Alpha chanel present
        end
      end;
    end;
  end;  // Draw

begin
if (HLib=0) or not FileExists(FileName) then
  begin Result:=false; exit end;
Data     := nil;
RowPtrs  := nil;
FHeight  := 0;
FWidth   := 0;
Load;
Draw;
if Data <> nil then FreeMem(Data);
if RowPtrs <> nil then FreeMem(RowPtrs);
result:=true
end;

initialization
HLib:=LoadLibrary(Lib);
if HLib>0 then
  begin
  png_set_expand:=GetProcAddress(HLib,'png_set_expand');
  png_set_invert_alpha:=GetProcAddress(HLib,'png_set_invert_alpha');
  png_set_swap:=GetProcAddress(HLib,'png_set_swap');
  png_set_packing:=GetProcAddress(HLib,'png_set_packing');
  png_close_file:=GetProcAddress(HLib,'png_close_file');
  png_open_file:=GetProcAddress(HLib,'png_open_file');
  png_create_info_struct:=GetProcAddress(HLib,'png_create_info_struct');
  png_init_io:=GetProcAddress(HLib,'png_init_io');
  png_read_image:=GetProcAddress(HLib,'png_read_image');
  png_read_info:=GetProcAddress(HLib,'png_read_info');
  png_read_update_info:=GetProcAddress(HLib,'png_read_update_info');
  png_set_read_status_fn:=GetProcAddress(HLib,'png_set_read_status_fn');
  png_get_rowbytes:=GetProcAddress(HLib,'png_get_rowbytes');
  png_get_valid:=GetProcAddress(HLib,'png_get_valid');
  png_destroy_read_struct:=GetProcAddress(HLib,'png_destroy_read_struct');
  png_create_read_struct:=GetProcAddress(HLib,'png_create_read_struct');
  png_get_IHDR:=GetProcAddress(HLib,'png_get_IHDR');
  end;

finalization
if HLib>0 then FreeLibrary(HLib);

end.

