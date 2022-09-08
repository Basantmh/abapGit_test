*************************************************
* PROGRAM ID           : ZPR_INT_UPLOAD_TOP     *
* AUTHOR               : Prakash Ranjan         *
* DATE                 : 17/04/2022             *
* DESCRIPTION          : Top Include            *
*-----------------------------------------------*

*&----------------------------------------------*
*&  Global Variables
*&----------------------------------------------*

DATA: v_infty    TYPE infty,
      v_ppnnn    TYPE ppnnn,
      v_tabname  TYPE tabname,
      v_filename TYPE string.

DATA: o_int_upload TYPE REF TO zpr_int_upload.

DATA: o_finaltab  TYPE REF TO data,
      o_finalline TYPE REF TO data.

FIELD-SYMBOLS: <fs_finaltab>  TYPE STANDARD TABLE,
               <fs_finalline> TYPE any.

DATA: i_msg_log TYPE bapirettab.

*&----------------------------------------------*
*&  Selection Screen
*&----------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

PARAMETERS: p_type TYPE zpr_data_type.

PARAMETERS: p_file TYPE localfile.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.

PARAMETERS: p_test TYPE xfeld DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK b2.
