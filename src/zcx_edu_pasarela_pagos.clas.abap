CLASS zcx_edu_pasarela_pagos DEFINITION
 PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA: gt_mensajes TYPE bapiret2_t.
    METHODS constructor
      IMPORTING
        type       TYPE  bapi_mtype
        id         TYPE  symsgid
        number     TYPE  symsgno
        message_v1 TYPE  symsgv OPTIONAL
        message_v2 TYPE  symsgv OPTIONAL
        message_v3 TYPE  symsgv OPTIONAL
        message_v4 TYPE  symsgv OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_edu_pasarela_pagos IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    DATA: ls_mensaje TYPE bapiret2.

    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.
    ls_mensaje-type = type.
    ls_mensaje-id = id.
    ls_mensaje-number = number.
    ls_mensaje-message_v1 = message_v1.
    ls_mensaje-message_v2 = message_v2.
    ls_mensaje-message_v3 = message_v3.
    ls_mensaje-message_v4 = message_v4.


    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = ls_mensaje-id
        msgnr               = ls_mensaje-number
        msgv1               = ls_mensaje-message_v1
        msgv2               = ls_mensaje-message_v2
        msgv3               = ls_mensaje-message_v3
        msgv4               = ls_mensaje-message_v4
      IMPORTING
        message_text_output = ls_mensaje-message.

    APPEND ls_mensaje TO me->gt_mensajes.
  ENDMETHOD.
ENDCLASS.
