class ZPR_INT_UPLOAD definition
  public
  final
  create public .

public section.

  constants CC_MSGID type MSGID value 'ZPR_INT_MSG' ##NO_TEXT.

  class-methods VALUE_REQ_TYPE
    exporting
      !EX_V_TYPE type ZPR_DATA_TYPE
    exceptions
      ERROR_IN_F4 .
  methods CREATE_DYNAMIC_TABLE
    importing
      !IM_V_TABNAME type TABNAME
    exporting
      !EX_O_FINALTAB type ref to DATA
      !EX_O_FINALLINE type ref to DATA
    exceptions
      ERROR_DYNAMIC_TABLE .
  methods UPDATE_DATA_INFTY
    importing
      !IM_V_TEST type XFELD
      !IM_V_INFTY type INFTY
      !IM_I_FINALTABLE type STANDARD TABLE
    changing
      !CH_WA_FINALLINE type ANY
      !CH_T_MSG_LOG type BAPIRETTAB .
  methods DISPLAY_ALV
    importing
      !IM_V_INFTY type INFTY
    changing
      !CH_I_FINALTABLE type STANDARD TABLE .
  methods UPLOAD_FILE
    importing
      !IM_V_FILENAME type STRING
    changing
      !CH_I_FINALTABLE type STANDARD TABLE
    exceptions
      ERROR_READING_FILE .
  methods WRITE_APP_LOG
    importing
      !IM_T_MSG_LOG type BAPIRETTAB .
  methods DISPLAY_ALV_HEADER
    importing
      !IM_V_INFTY type INFTY
      !CH_O_ALV type ref to CL_SALV_TABLE .
  methods UPDATE_DATA_INFTY_NEW
    importing
      !IM_V_TEST type XFELD
      !IM_V_INFTY type INFTY
      !IM_I_FINALTABLE type STANDARD TABLE
    changing
      !CH_WA_FINALLINE type ANY
      !CH_T_MSG_LOG type BAPIRETTAB .
protected section.
private section.
ENDCLASS.



CLASS ZPR_INT_UPLOAD IMPLEMENTATION.


METHOD create_dynamic_table.
*************************************************
* PROGRAM ID           : CREATE_DYNAMIC_TABLE   *
* AUTHOR               : Prakash Ranjan         *
* DATE                 : 17/04/2022             *
* DESCRIPTION          : Create Dynamic Table   *
*-----------------------------------------------*

  DATA: l_o_struct     TYPE REF TO cl_abap_structdescr,
        l_o_out_type   TYPE REF TO cl_abap_structdescr,
        l_o_out_table  TYPE REF TO cl_abap_tabledescr,
        l_o_struct_tmp TYPE REF TO cl_abap_typedescr.

  DATA: l_i_components TYPE STANDARD TABLE OF abap_componentdescr WITH KEY name.

  CALL METHOD cl_abap_structdescr=>describe_by_name
    EXPORTING
      p_name         = im_v_tabname
    RECEIVING
      p_descr_ref    = l_o_struct_tmp
    EXCEPTIONS
      type_not_found = 1
      OTHERS         = 2.
  IF sy-subrc = 0 AND l_o_struct_tmp IS BOUND AND l_o_struct IS INITIAL.

    TRY.
        l_o_struct ?= l_o_struct_tmp.
      CATCH cx_sy_conversion_no_number
        cx_sy_conversion_overflow
        cx_sy_move_cast_error.
    ENDTRY.

    IF l_o_struct IS BOUND.
      l_i_components = l_o_struct->get_components( ).
    ELSE.
      RAISE error_dynamic_table.
    ENDIF.

  ELSE.
    RAISE error_dynamic_table.
  ENDIF.

  IF NOT l_i_components IS INITIAL.
    TRY.
        l_o_out_type = cl_abap_structdescr=>create( l_i_components ).
      CATCH cx_sy_struct_creation.
        RAISE error_dynamic_table.
    ENDTRY.

    TRY.
        l_o_out_table = cl_abap_tabledescr=>create(
        p_line_type = l_o_out_type
        p_table_kind = cl_abap_tabledescr=>tablekind_std
        p_unique = abap_false ).
      CATCH cx_sy_table_creation.
        RAISE error_dynamic_table.
    ENDTRY.

  ENDIF.

  CREATE DATA ex_o_finaltab  TYPE HANDLE l_o_out_table.
  CREATE DATA ex_o_finalline TYPE HANDLE l_o_out_type.

ENDMETHOD.


METHOD display_alv.
*************************************************
* PROGRAM ID           : DISPLAY_ALV            *
* AUTHOR               : Prakash Ranjan         *
* DATE                 : 17/04/2022             *
* DESCRIPTION          : Write Application Log  *
*-----------------------------------------------*

  DATA: l_o_alv        TYPE REF TO cl_salv_table,
        l_o_selections TYPE REF TO cl_salv_selections,
        l_o_columns    TYPE REF TO cl_salv_columns_table.

  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = l_o_alv
        CHANGING
          t_table      = ch_i_finaltable.
    CATCH cx_salv_msg .
  ENDTRY.

  CALL METHOD me->display_alv_header
    EXPORTING
      im_v_infty = im_v_infty
      ch_o_alv   = l_o_alv.

  l_o_selections = l_o_alv->get_selections( ).
  IF l_o_selections IS BOUND.
    l_o_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).
  ENDIF.

*  Set PF status
  CALL METHOD l_o_alv->set_screen_status
    EXPORTING
      report        = sy-cprog
      pfstatus      = 'STANDARD'
      set_functions = l_o_alv->c_functions_all.

* Optimize Columns
  l_o_columns = l_o_alv->get_columns( ).

  IF l_o_columns IS BOUND AND l_o_columns IS NOT INITIAL .
    l_o_columns->set_optimize( abap_true ).
  ENDIF.

  IF l_o_alv IS BOUND.
    CALL METHOD l_o_alv->display.
  ENDIF.

ENDMETHOD.


METHOD display_alv_header.
*************************************************
* PROGRAM ID           : DISPLAY_ALV_HEADER     *
* AUTHOR               : Prakash Ranjan         *
* DATE                 : 17/04/2022             *
* DESCRIPTION          : Display ALV Header     *
*-----------------------------------------------*

    DATA : l_o_layout_grid TYPE REF TO cl_salv_form_layout_grid, "Grid Element in Design Object
           l_o_layout_flow TYPE REF TO cl_salv_form_layout_flow, "Flow Element in Design Object
           l_o_form_label  TYPE REF TO cl_salv_form_label,       "Element of Type Label
*Internal Table declaration
           l_i_header      TYPE slis_t_listheader,               "Header table
*Variable declaration
           l_cnt_row       TYPE counter_int4.              "Counter

    CONSTANTS : l_c_1 TYPE row_length VALUE 1,  "Column/row number
                l_c_2 TYPE row_length VALUE 2.  "Column/row number

    FIELD-SYMBOLS : <l_fs_header> TYPE slis_listheader.

*   Create layout grid
    IF  l_o_layout_grid IS NOT BOUND.
      TRY.
          CREATE OBJECT : l_o_layout_grid.
        CATCH cx_sy_create_object_error
              cx_root.
* No error message population is requred here. bound check is done while calling method
      ENDTRY.
    ENDIF.

    l_i_header = VALUE #( BASE l_i_header ( typ = 'H' key = TEXT-001 info = sy-cprog ) ).
    l_i_header = VALUE #( BASE l_i_header ( typ = 'H' key = TEXT-002 info = im_v_infty ) ).

    LOOP AT l_i_header ASSIGNING <l_fs_header>.

      TRY.
          l_cnt_row = l_cnt_row + 1.
        CATCH  cx_sy_arithmetic_overflow
                      cx_sy_conversion_overflow
                      cx_sy_arithmetic_error
                      cx_root.
      ENDTRY.
      IF l_o_layout_grid IS BOUND.
*Create label element in cell
        CALL METHOD l_o_layout_grid->create_label
          EXPORTING
            row     = l_cnt_row
            column  = l_c_1
          RECEIVING
            r_value = l_o_form_label.
      ENDIF.
      IF l_o_form_label IS BOUND.
*Define Text of an Element
        CALL METHOD l_o_form_label->set_text
          EXPORTING
            value = <l_fs_header>-key.
      ENDIF.
      IF  l_o_layout_grid IS BOUND.
*Create Flow Element in Cell
        CALL METHOD l_o_layout_grid->create_flow
          EXPORTING
            row     = l_cnt_row
            column  = l_c_2
          RECEIVING
            r_value = l_o_layout_flow.
      ENDIF.
      IF l_o_layout_flow IS BOUND.
*Create Flow Element for Position
        CALL METHOD l_o_layout_flow->create_text
          EXPORTING
            text = <l_fs_header>-info.
      ENDIF.

    ENDLOOP.
    ch_o_alv->set_top_of_list( l_o_layout_grid ).

  ENDMETHOD.


METHOD update_data_infty.
*************************************************
* PROGRAM ID           : UPDATE_DATA_INFTY      *
* AUTHOR               : Prakash Ranjan         *
* DATE                 : 17/04/2022             *
* DESCRIPTION          : Update Data Infotype   *
*-----------------------------------------------*

  DATA: l_v_pernr TYPE pernr_d,
        l_v_subty TYPE subty,
        l_v_begda TYPE begda,
        l_v_endda TYPE endda,
        l_v_objps TYPE objps,
        l_v_sprps TYPE sprps,
        l_v_seqnr TYPE seqnr.

  DATA: l_wa_return TYPE bapireturn1,
        l_wa_key    TYPE bapipakey.

  FIELD-SYMBOLS: <l_fs_any> TYPE any.

  LOOP AT im_i_finaltable ASSIGNING FIELD-SYMBOL(<l_fs_finalline>).

    CLEAR: ch_wa_finalline.
    MOVE-CORRESPONDING <l_fs_finalline> TO ch_wa_finalline. "Convert from PAxxxx to Pxxxx Structure

    ASSIGN COMPONENT 'INFTY' OF STRUCTURE ch_wa_finalline TO <l_fs_any>.
    IF sy-subrc = 0.
      <l_fs_any> = im_v_infty.
    ENDIF.
    ASSIGN COMPONENT 'PERNR' OF STRUCTURE ch_wa_finalline TO <l_fs_any>.
    IF sy-subrc = 0.
      l_v_pernr = <l_fs_any>.
    ENDIF.
    ASSIGN COMPONENT 'SUBTY' OF STRUCTURE ch_wa_finalline TO <l_fs_any>.
    IF sy-subrc = 0.
      l_v_subty = <l_fs_any>.
    ENDIF.
    ASSIGN COMPONENT 'BEGDA' OF STRUCTURE ch_wa_finalline TO <l_fs_any>.
    IF sy-subrc = 0.
      l_v_begda = <l_fs_any>.
    ENDIF.
    ASSIGN COMPONENT 'ENDDA' OF STRUCTURE ch_wa_finalline TO <l_fs_any>.
    IF sy-subrc = 0.
      l_v_endda = <l_fs_any>.
    ENDIF.
    ASSIGN COMPONENT 'OBJPS' OF STRUCTURE ch_wa_finalline TO <l_fs_any>.
    IF sy-subrc = 0.
      l_v_objps = <l_fs_any>.
    ENDIF.
    ASSIGN COMPONENT 'SPRPS' OF STRUCTURE ch_wa_finalline TO <l_fs_any>.
    IF sy-subrc = 0.
      l_v_sprps = <l_fs_any>.
    ENDIF.
    ASSIGN COMPONENT 'SEQNR' OF STRUCTURE ch_wa_finalline TO <l_fs_any>.
    IF sy-subrc = 0.
      l_v_seqnr = <l_fs_any>.
    ENDIF.

    CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = l_v_pernr
      IMPORTING
        return = l_wa_return.

    IF NOT l_wa_return IS INITIAL.
      ch_t_msg_log = VALUE #( BASE ch_t_msg_log ( type = l_wa_return-type id = cc_msgid number = '000'
      message_v1 = im_v_infty message_v2 = l_v_pernr message_v3 = l_v_subty message_v4 = l_wa_return-message ) ).
    ELSE.

      CALL FUNCTION 'HR_CONTROL_INFTY_OPERATION'
        EXPORTING
          infty         = im_v_infty
          number        = l_v_pernr
          subtype       = l_v_subty
          objectid      = l_v_objps
          lockindicator = l_v_sprps
          validityend   = l_v_endda
          validitybegin = l_v_begda
          recordnumber  = l_v_seqnr
          record        = ch_wa_finalline
          operation     = 'INS'
          nocommit      = abap_true
        IMPORTING
          return        = l_wa_return
          key           = l_wa_key.

      IF NOT l_wa_return IS INITIAL.
        ch_t_msg_log = VALUE #( BASE ch_t_msg_log ( type = l_wa_return-type id = cc_msgid number = '001'
        message_v1 = im_v_infty message_v2 = l_v_pernr message_v3 = l_v_subty message_v4 = l_wa_return-message ) ).
      ENDIF.

      IF l_wa_return-type = 'E' OR l_wa_return-type = 'A'.
        "Error
        ch_t_msg_log = VALUE #( BASE ch_t_msg_log ( type = l_wa_return-type id = cc_msgid number = '009'
        message_v1 = im_v_infty message_v2 = l_v_pernr message_v3 = l_v_subty message_v4 = l_wa_return-message ) ).
      ELSE.
        "Success
        ch_t_msg_log = VALUE #( BASE ch_t_msg_log ( type = l_wa_return-type id = cc_msgid number = '008'
        message_v1 = im_v_infty message_v2 = l_v_pernr message_v3 = l_v_subty message_v4 = l_wa_return-message ) ).
      ENDIF.

      IF im_v_test IS INITIAL.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.

    ENDIF.

    CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = l_v_pernr
      IMPORTING
        return = l_wa_return.

  ENDLOOP.

ENDMETHOD.


METHOD update_data_infty_new.
*************************************************
* PROGRAM ID           : UPDATE_DATA_INFTY_NEW  *
* AUTHOR               : Prakash Ranjan         *
* DATE                 : 17/04/2022             *
* DESCRIPTION          : Update Data Infotype   *
*-----------------------------------------------*

  DATA: l_v_pernr TYPE pernr_d,
        l_v_subty TYPE subty,
        l_v_begda TYPE begda,
        l_v_endda TYPE endda,
        l_v_objps TYPE objps,
        l_v_sprps TYPE sprps,
        l_v_seqnr TYPE seqnr.

  DATA: l_wa_return TYPE bapireturn1,
        l_wa_key    TYPE bapipakey.

  DATA: l_o_hr_data TYPE REF TO if_hrpa_masterdata_bl.

  FIELD-SYMBOLS: <l_fs_any> TYPE any.

  LOOP AT im_i_finaltable ASSIGNING FIELD-SYMBOL(<l_fs_finalline>).

    CLEAR: ch_wa_finalline.
    MOVE-CORRESPONDING <l_fs_finalline> TO ch_wa_finalline. "Convert from PAxxxx to Pxxxx Structure

    CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = l_v_pernr
      IMPORTING
        return = l_wa_return.

    IF NOT l_wa_return IS INITIAL.
      ch_t_msg_log = VALUE #( BASE ch_t_msg_log ( type = l_wa_return-type id = cc_msgid number = '000'
      message_v1 = im_v_infty message_v2 = l_v_pernr message_v3 = l_v_subty message_v4 = l_wa_return-message ) ).
    ELSE.

      CALL METHOD cl_hrsfi_comp_utilities=>infotype_insert
        EXPORTING
          is_pnnnn    = ch_wa_finalline
        IMPORTING
          ev_error    = DATA(l_v_error)
          et_messages = DATA(l_i_message).

      IF l_v_error IS INITIAL.
        "Success
        ch_t_msg_log = VALUE #( BASE ch_t_msg_log ( type = l_wa_return-type id = cc_msgid number = '008'
        message_v1 = im_v_infty message_v2 = l_v_pernr message_v3 = l_v_subty message_v4 = l_wa_return-message ) ).
      ELSE.
        "Error
        ch_t_msg_log = VALUE #( BASE ch_t_msg_log ( type = l_wa_return-type id = cc_msgid number = '009'
        message_v1 = im_v_infty message_v2 = l_v_pernr message_v3 = l_v_subty message_v4 = l_wa_return-message ) ).
      ENDIF.

      LOOP AT l_i_message INTO l_wa_return.
        ch_t_msg_log = VALUE #( BASE ch_t_msg_log ( type = l_wa_return-type id = cc_msgid number = '001'
        message_v1 = im_v_infty message_v2 = l_v_pernr message_v3 = l_v_subty message_v4 = l_wa_return-message ) ).
      ENDLOOP.

      IF im_v_test IS INITIAL AND l_v_error IS INITIAL.
        cl_hrpa_masterdata_factory=>get_business_logic( IMPORTING business_logic = l_o_hr_data ).

        IF l_o_hr_data IS BOUND.
          l_o_hr_data->flush( EXPORTING no_commit = abap_false ).
        ENDIF.
      ELSE.
        ROLLBACK WORK.
      ENDIF.

    ENDIF.

    CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = l_v_pernr
      IMPORTING
        return = l_wa_return.

    CLEAR: l_v_error, l_i_message.

  ENDLOOP.

ENDMETHOD.


METHOD upload_file.
*************************************************
* PROGRAM ID           : UPLOAD_FILE            *
* AUTHOR               : Prakash Ranjan         *
* DATE                 : 17/04/2022             *
* DESCRIPTION          : Upload & Parse File    *
*-----------------------------------------------*

    DATA: l_i_rawdata TYPE  truxs_t_text_data.

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = im_v_filename
      TABLES
        data_tab                = l_i_rawdata
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.
    IF sy-subrc <> 0.
      RAISE error_reading_file.
    ELSE.
      CALL FUNCTION 'TEXT_CONVERT_TEX_TO_SAP'
        EXPORTING
          i_field_seperator    = '|'
          i_line_header        = abap_true
          i_tab_raw_data       = l_i_rawdata
        TABLES
          i_tab_converted_data = ch_i_finaltable
        EXCEPTIONS
          conversion_failed    = 1
          OTHERS               = 2.
      IF sy-subrc <> 0.
        RAISE error_reading_file.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD value_req_type.
*************************************************
* PROGRAM ID           : VALUE_REQ_TYPE         *
* AUTHOR               : Prakash Ranjan         *
* DATE                 : 17/04/2022             *
* DESCRIPTION          : Value Request          *
*-----------------------------------------------*

    TYPES: BEGIN OF l_ty_int_up,
             type  TYPE zpr_data_type,
             infty TYPE infty,
           END OF l_ty_int_up.

    DATA: l_i_type_tab      TYPE STANDARD TABLE OF l_ty_int_up,
          l_i_return_fields TYPE STANDARD TABLE OF ddshretval.

* Custom Table
    SELECT type infty
      FROM zpr_int_up
      INTO TABLE l_i_type_tab.

    IF sy-subrc = 0.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = 'TYPE'
          dynpprog        = sy-repid
          dynpnr          = sy-dynnr
          window_title    = TEXT-001
          value_org       = 'S'
        TABLES
          value_tab       = l_i_type_tab
          return_tab      = l_i_return_fields
        EXCEPTIONS
          parameter_error = 1
          no_values_found = 2
          OTHERS          = 3.
      IF sy-subrc = 0.
        READ TABLE l_i_return_fields ASSIGNING FIELD-SYMBOL(<l_fs_return_fields>) INDEX 1.
        IF sy-subrc = 0.
          ex_v_type = <l_fs_return_fields>-fieldval.
        ELSE.
          RAISE error_in_f4.
        ENDIF.
      ELSE.
        RAISE error_in_f4.
      ENDIF.
    ELSE.
      RAISE error_in_f4.
    ENDIF.

  ENDMETHOD.


METHOD write_app_log.
*************************************************
* PROGRAM ID           : WRITE_APP_LOG          *
* AUTHOR               : Prakash Ranjan         *
* DATE                 : 17/04/2022             *
* DESCRIPTION          : Write Application Log  *
*-----------------------------------------------*
  DATA: l_v_log_handle TYPE balloghndl,
        l_wa_log       TYPE bal_s_log,
        l_wa_msg       TYPE bal_s_msg.

  DATA: l_is_log_handle TYPE bal_t_logh.

  l_wa_log-object = 'ZOBJ'.
  l_wa_log-subobject = 'ZSUB_OBJ'.
  l_wa_log-aldate = sy-datum.
  l_wa_log-altime = sy-uzeit.
  l_wa_log-aluser = sy-uname.
  l_wa_log-altcode = sy-tcode.
  l_wa_log-alprog = sy-repid.

      l_wa_log-aldate_del = sy-datum + 365. "Log Retention

  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log                 = l_wa_log
    IMPORTING
      e_log_handle            = l_v_log_handle
    EXCEPTIONS
      log_header_inconsistent = 1
      OTHERS                  = 2.
  IF sy-subrc = 0.

    LOOP AT im_t_msg_log ASSIGNING FIELD-SYMBOL(<l_fs_mgs_log>).

      CLEAR: l_wa_msg.

      l_wa_msg-msgty = <l_fs_mgs_log>-type.
      l_wa_msg-msgid = <l_fs_mgs_log>-id.
      l_wa_msg-msgno = <l_fs_mgs_log>-number.
      l_wa_msg-msgv1 = <l_fs_mgs_log>-message_v1.
      l_wa_msg-msgv2 = <l_fs_mgs_log>-message_v2.
      l_wa_msg-msgv3 = <l_fs_mgs_log>-message_v3.
      l_wa_msg-msgv4 = <l_fs_mgs_log>-message_v4.

      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle     = l_v_log_handle
          i_s_msg          = l_wa_msg
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          OTHERS           = 4.
*Error handling is not required, program flow will continue
    ENDLOOP.

    INSERT l_v_log_handle INTO TABLE l_is_log_handle.
    CLEAR: l_v_log_handle.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_t_log_handle   = l_is_log_handle
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDIF.

ENDMETHOD.
ENDCLASS.
