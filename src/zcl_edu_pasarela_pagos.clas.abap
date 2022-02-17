CLASS zcl_edu_pasarela_pagos DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor,
      create_request
        EXPORTING
          e_output TYPE zstedu_redirectresponse_out
        CHANGING
          i_input  TYPE zstedu_redirectrequest_in
        RAISING
          zcx_edu_pasarela_pagos,

      get_request_information
        IMPORTING
          i_request_id TYPE string
        EXPORTING
          e_output     TYPE zstedu_redirectinformation_out
        CHANGING
          i_input      TYPE zstedu_redirectinformation_in
        RAISING
          zcx_edu_pasarela_pagos,

      get_auth
        IMPORTING
          i_secret_key TYPE string
        EXPORTING
          e_tran_key   TYPE string
          e_nonce      TYPE string
          e_seed_start TYPE string
          e_seed_end   TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: gt_c_param    TYPE SORTED TABLE OF zedu_c_param WITH UNIQUE KEY primary_key COMPONENTS repid idparam idparampos.

    CONSTANTS: gc_content_type TYPE string VALUE 'Content-type',
               gc_json_content TYPE string VALUE 'application/json;charset=UTF-8',
               gc_repid_pp     TYPE char40 VALUE 'ZCL_EDU_PASARELA_PAGOS',
               gc_rest_api     TYPE char8 VALUE 'REST_API',
               gc_rfc_dest     TYPE char8 VALUE 'RFC_DEST',
               gc_login        TYPE char8 VALUE 'LOGIN',
               gc_secret_key   TYPE char10 VALUE 'SECRET_KEY',
               gc_return_url   TYPE char10 VALUE 'RETURN_URL',
               gc_allow_part   TYPE char10 VALUE 'ALLOW_PART'.


    METHODS:
      http_client IMPORTING i_in          TYPE any  "DATA "TYPE REF TO data
                            i_destination TYPE c OPTIONAL
                            i_request_id  TYPE string OPTIONAL
                  EXPORTING e_out         TYPE any  "REF TO data
                            e_status      TYPE i
                  RAISING   zcx_edu_pasarela_pagos,

      get_customizing IMPORTING i_matricula TYPE check OPTIONAL.

ENDCLASS.



CLASS zcl_edu_pasarela_pagos IMPLEMENTATION.


  METHOD constructor.
    me->get_customizing( ).
  ENDMETHOD.


  METHOD create_request.
    DATA: lo_http_client  TYPE REF TO if_http_client,
          lo_conv         TYPE REF TO cl_abap_conv_in_ce,
          lv_res_data_bin TYPE xstring,
          lv_res_data_str TYPE string,
          lv_req_data_bin TYPE xstring,
          lv_req_data_str TYPE string,
          lv_code         TYPE i.
*          i_input_1       TYPE zstedu_createrequest_in.

    i_input-auth-login = me->gt_c_param[ repid = gc_repid_pp idparam = gc_login ]-valor.

    me->get_auth(
      EXPORTING
        i_secret_key = CONV string( me->gt_c_param[ repid = gc_repid_pp idparam = gc_secret_key ]-valor )
      IMPORTING
        e_tran_key   = i_input-auth-tran_key
        e_nonce      = i_input-auth-nonce
        e_seed_start = i_input-auth-seed
        e_seed_end   = i_input-expiration
    ).

    i_input-return_url = me->gt_c_param[ repid = gc_repid_pp idparam = gc_return_url ]-valor.
    i_input-ip_address = '127.0.0.1'.
    i_input-user_agent = |'SAP NETWEAVER 7.4 '{ sy-saprl } '-' { sy-opsys }|.
    i_input-payment-allowpartial = me->gt_c_param[ repid = gc_repid_pp idparam = gc_allow_part ]-valor.


    me->http_client(
      EXPORTING
        i_in                   = i_input
        i_destination          = me->gt_c_param[ repid = gc_repid_pp idparam = gc_rfc_dest ]-valor
      IMPORTING
        e_out                  = e_output
        e_status               = DATA(lv_status_rest)
    ).


    IF e_output-status-status NE 'OK'.

      RAISE EXCEPTION TYPE zcx_edu_pasarela_pagos
        EXPORTING
          type       = 'E'
          id         = 'ZDEDU_FACTURACION'
          number     = '000'
          message_v1 = CONV symsgv( e_output-status-status )
          message_v2 = CONV symsgv( e_output-status-message )
          message_v3 = CONV symsgv( e_output-status-reason )
          message_v4 = CONV symsgv( e_output-status-date ).
    ENDIF.

  ENDMETHOD.


  METHOD get_auth.

    DATA lo_digest TYPE REF TO cl_abap_message_digest.
    DATA:lv_nonce       TYPE string,
         lv_xnonce      TYPE xstring,
         lv_hash_string TYPE string,
         lv_hash_base64 TYPE string.

    TRY.
        GET TIME STAMP FIELD DATA(lv_timestamp).

        e_seed_start = |{ lv_timestamp  TIMESTAMP = ISO TIMEZONE = sy-zonlo }|.

        DATA(lv_seed_end) = |{ cl_abap_tstmp=>add( tstmp  = lv_timestamp secs  = ( 10 * 60 ) ) TIMESTAMP = ISO TIMEZONE = sy-zonlo }|.
        e_seed_end = lv_seed_end(19).
        CALL FUNCTION 'GENERAL_GET_RANDOM_STRING'
          EXPORTING
            number_chars  = 24
          IMPORTING
            random_string = lv_nonce.


        cl_abap_message_digest=>calculate_hash_for_char(
          EXPORTING
            if_algorithm = 'SHA1'
            if_data = |{ lv_nonce }{ e_seed_start }{ i_secret_key }|
          IMPORTING
            ef_hashstring = lv_hash_string
            ef_hashb64string = e_tran_key
        ).

        cl_bcs_convert=>string_to_xstring(
          EXPORTING
            iv_string     = lv_nonce
          RECEIVING
            ev_xstring    = lv_xnonce
        ).

        CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
          EXPORTING
            input  = lv_xnonce
          IMPORTING
            output = e_nonce.

      CATCH cx_abap_message_digest INTO DATA(lcx_abap_message_digest).
      CATCH cx_bcs INTO DATA(lcx_bcs).
    ENDTRY.
  ENDMETHOD.


  METHOD get_customizing.

    SELECT mandt repid idparam idparampos valor
      INTO TABLE me->gt_c_param
      FROM zedu_c_param
      WHERE repid = gc_repid_pp.

  ENDMETHOD.


  METHOD get_request_information.

    i_input-auth-login = me->gt_c_param[ repid = gc_repid_pp idparam = gc_login ]-valor.

    me->get_auth(
      EXPORTING
        i_secret_key = CONV string( me->gt_c_param[ repid = gc_repid_pp idparam = gc_secret_key ]-valor )
      IMPORTING
        e_tran_key   = i_input-auth-tran_key
        e_nonce      = i_input-auth-nonce
        e_seed_start = i_input-auth-seed
    ).


    me->http_client(
       EXPORTING
         i_in                   = i_input
         i_destination          = me->gt_c_param[ repid = gc_repid_pp idparam = gc_rfc_dest ]-valor
         i_request_id          = i_request_id
       IMPORTING
         e_out                  = e_output
         e_status               = DATA(lv_status_rest)
     ).

    IF e_output-status-status EQ 'REJECTED' OR
       e_output-status-status EQ 'APPROVED_PARTIAL' OR
       e_output-status-status EQ 'PARTIAL_EXPIRED'.

      RAISE EXCEPTION TYPE zcx_edu_pasarela_pagos
        EXPORTING
          type       = 'E'
          id         = 'ZDEDU_FACTURACION'
          number     = '000'
          message_v1 = CONV symsgv( e_output-status-status )
          message_v2 = CONV symsgv( e_output-status-reason )
          message_v3 = CONV symsgv( e_output-status-message )
          message_v4 = CONV symsgv( e_output-status-date ).
    ENDIF.

  ENDMETHOD.


  METHOD http_client.

    DATA: lo_http_client  TYPE REF TO if_http_client.
    DATA: lv_req_data_bin TYPE xstring.
    DATA(lv_sjson_in) = /ui2/cl_json=>serialize( data = i_in compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

    cl_http_client=>create_by_destination(
      EXPORTING
        destination              = i_destination
      IMPORTING
        client                   = lo_http_client
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 6
    ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_edu_pasarela_pagos
        EXPORTING
          type       = sy-msgty
          id         = sy-msgid
          number     = sy-msgno
          message_v1 = sy-msgv1
          message_v2 = sy-msgv2
          message_v3 = sy-msgv3
          message_v4 = sy-msgv4.
    ENDIF.

    IF lo_http_client IS BOUND.
      DATA  lt_fields  TYPE tihttpnvp.

      lo_http_client->request->get_header_fields(
        CHANGING
          fields =  lt_fields
      ).

      lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).
      lo_http_client->request->set_method( if_http_request=>co_request_method_post ).
      lo_http_client->request->set_content_type( gc_json_content ).
      IF i_request_id IS NOT INITIAL.
        cl_http_utility=>set_request_uri( request = lo_http_client->request
                                          uri     =  `/` && i_request_id ).
      ENDIF.

      TRY.
          cl_bcs_convert=>string_to_xstring(
            EXPORTING
              iv_string     = lv_sjson_in
            RECEIVING
              ev_xstring    = lv_req_data_bin
          ).
        CATCH cx_bcs INTO DATA(lx_bcs).
      ENDTRY.

      lo_http_client->request->set_data( data = lv_req_data_bin ).

      lo_http_client->send(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          http_invalid_timeout       = 4
          OTHERS                     = 5
      ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_edu_pasarela_pagos
          EXPORTING
            type       = sy-msgty
            id         = sy-msgid
            number     = sy-msgno
            message_v1 = sy-msgv1
            message_v2 = sy-msgv2
            message_v3 = sy-msgv3
            message_v4 = sy-msgv4.
      ENDIF.

      lo_http_client->receive(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4
      ).

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_edu_pasarela_pagos
          EXPORTING
            type       = sy-msgty
            id         = sy-msgid
            number     = sy-msgno
            message_v1 = sy-msgv1
            message_v2 = sy-msgv2
            message_v3 = sy-msgv3
            message_v4 = sy-msgv4.
      ENDIF.

      DATA(lv_cdata) = lo_http_client->response->get_cdata( ).

      lo_http_client->close( ).

      lo_http_client->response->get_status( IMPORTING code = e_status ).

      /ui2/cl_json=>deserialize( EXPORTING json = lv_cdata pretty_name = /ui2/cl_json=>pretty_mode-camel_case CHANGING data = e_out ).

    ENDIF.
  ENDMETHOD.
ENDCLASS.
