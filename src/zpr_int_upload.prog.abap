*************************************************
* PROGRAM ID           : ZPR_INT_UPLOAD         *
* AUTHOR               : Prakash Ranjan         *
* DATE                 : 17/04/2022             *
* DESCRIPTION          : Upload Utility         *
*-----------------------------------------------*
REPORT zpr_int_upload MESSAGE-ID zpr_int_msg.

* Top Include, Declaration, Selection Screen
INCLUDE zpr_int_upload_top.

INITIALIZATION.

  CREATE OBJECT o_int_upload.
  IF o_int_upload IS NOT BOUND.
    MESSAGE e003. "Technical Issue in creating object instance.
  ENDIF.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_type.
* Get Data Types
  CALL METHOD zpr_int_upload=>value_req_type
    IMPORTING
      ex_v_type   = p_type
    EXCEPTIONS
      error_in_f4 = 1
      OTHERS      = 2.
  IF sy-subrc <> 0.
    MESSAGE i004. "Failed to retrieve data types. Please check configurations in ZPR_INT_UP.
    LEAVE LIST-PROCESSING.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = p_file.

START-OF-SELECTION.

  SELECT SINGLE infty ppnnn dbtabname INTO (v_infty , v_ppnnn, v_tabname)
    FROM zpr_int_up WHERE type = p_type.
  IF sy-subrc <> 0 OR v_ppnnn IS INITIAL OR v_tabname IS INITIAL.
    MESSAGE e005 WITH p_type. "Configuration in Table ZPR_INT_UP is incomplete for &1.
  ENDIF.

END-OF-SELECTION.

* Create Dynamic Table PAxxxx
  CALL METHOD o_int_upload->create_dynamic_table
    EXPORTING
      im_v_tabname        = v_tabname
    IMPORTING
      ex_o_finaltab       = o_finaltab
    EXCEPTIONS
      error_dynamic_table = 1
      OTHERS              = 2.
  IF sy-subrc <> 0.
    MESSAGE e006. "Dynamic Table could not be created. Please ensure proper configurations.
  ENDIF.

  TRY.
      ASSIGN o_finaltab->* TO <fs_finaltab>.
    CATCH
      cx_sy_assign_cast_illegal_cast
      cx_sy_assign_cast_unknown_type
      cx_sy_assign_out_of_range.
      MESSAGE e006. "Dynamic Table could not be created. Please ensure proper configurations.
  ENDTRY.


* Create Dynamic Structure Pxxxx
  CALL METHOD o_int_upload->create_dynamic_table
    EXPORTING
      im_v_tabname        = v_ppnnn
    IMPORTING
      ex_o_finalline      = o_finalline
    EXCEPTIONS
      error_dynamic_table = 1
      OTHERS              = 2.
  IF sy-subrc <> 0.
    MESSAGE e006. "Dynamic Table could not be created. Please ensure proper configurations.
  ENDIF.

  TRY.
      ASSIGN o_finalline->* TO <fs_finalline>.
    CATCH
      cx_sy_assign_cast_illegal_cast
      cx_sy_assign_cast_unknown_type
      cx_sy_assign_out_of_range.
      MESSAGE e006. "Dynamic Table could not be created. Please ensure proper configurations.
  ENDTRY.

  v_filename = p_file.

  IF <fs_finaltab> IS ASSIGNED.
    CALL METHOD o_int_upload->upload_file
      EXPORTING
        im_v_filename      = v_filename
      CHANGING
        ch_i_finaltable    = <fs_finaltab>
      EXCEPTIONS
        error_reading_file = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      MESSAGE e007. "File upload failed. Please ensure file is not open on desktop.
    ENDIF.
  ENDIF.

* Upload Data Into Infotypes
  IF <fs_finaltab> IS ASSIGNED AND <fs_finalline> IS ASSIGNED.
    CALL METHOD o_int_upload->update_data_infty_new
      EXPORTING
        im_v_test       = p_test
        im_v_infty      = v_infty
        im_i_finaltable = <fs_finaltab>
      CHANGING
        ch_wa_finalline = <fs_finalline>
        ch_t_msg_log    = i_msg_log.

* Write Application Log
    CALL METHOD o_int_upload->write_app_log
      EXPORTING
        im_t_msg_log = i_msg_log.

    IF NOT p_test IS INITIAL.
* Display ALV
      CALL METHOD o_int_upload->display_alv
        EXPORTING
          im_v_infty      = v_infty
        CHANGING
          ch_i_finaltable = <fs_finaltab>.
    ELSE.
      MESSAGE i002. "Data has been uploaded, Please check logs for more details.
    ENDIF.

  ENDIF.
