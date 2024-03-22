*----------------------------------------------------------------------*
***INCLUDE LZFG_CITEK_POCF01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  PROCESS_FC_DESIGN_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PROCESS_FC_DESIGN_TABLE .
  FIELD-SYMBOLS:
    <lf_templ> TYPE ZVI_BM_RPC_SI-TEMPL.

  LOOP AT EXTRACT.
    ZVI_BM_RPC_SI_EXTRACT = EXTRACT.
    CHECK ZVI_BM_RPC_SI_EXTRACT-MARK IS NOT INITIAL.
    MOVE-CORRESPONDING ZVI_BM_RPC_SI_EXTRACT TO ZVI_BM_RPC_SI.
    CALL FUNCTION 'ZFM_BM_RPC_DESIGN_SEGITM'
      EXPORTING
        I_RPC_SI = ZVI_BM_RPC_SI.

    SELECT SINGLE TEMPL
      from ZTB_BM_RPC_SI
      INTO ZVI_BM_RPC_SI_EXTRACT-TEMPL
     WHERE REPID = ZVI_BM_RPC_SI-REPID
       AND RPSEG = ZVI_BM_RPC_SI-RPSEG
       AND RITEM = ZVI_BM_RPC_SI-RITEM.
    EXTRACT = ZVI_BM_RPC_SI_EXTRACT.
    MODIFY EXTRACT.
    EXIT.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PROCESS_FC_DESIGN_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PROCESS_FC_INPUT_MANUAL.

  LOOP AT EXTRACT.
    ZVI_BM_RPC_F_EXTRACT = EXTRACT.
    CHECK ZVI_BM_RPC_F_EXTRACT-MARK IS NOT INITIAL.
    MOVE-CORRESPONDING ZVI_BM_RPC_F_EXTRACT TO ZVI_BM_RPC_F.
    CALL FUNCTION 'ZFM_BM_RPC_INPUT_MANUAL_DATA'
      EXPORTING
        I_RPC_F = ZVI_BM_RPC_F.
    EXIT.
  ENDLOOP.

ENDFORM.

*{   INSERT         DEVK909644                                        1
*&---------------------------------------------------------------------*
*& Form PROCESS_FC_EDIT_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM PROCESS_FC_EDIT_DF .
*PER
  DATA:
        LW_NAME    TYPE THEAD-TDNAME,
        LW_COUNT   TYPE I,
        LW_MODE    TYPE CHAR1,

        LS_EXTRACT LIKE ZVI_BM_RPC_W_EXTRACT,
        LS_header  TYPE THEAD,

        LT_LINES   TYPE TABLE OF TLINE
        .
  LOOP AT EXTRACT.
    ZVI_BM_RPC_W_EXTRACT = EXTRACT.
    IF ZVI_BM_RPC_W_EXTRACT-MARK IS NOT INITIAL..
      LW_COUNT  = LW_COUNT + 1.
      LS_EXTRACT = ZVI_BM_RPC_W_EXTRACT.
    ENDIF.
  ENDLOOP.
  IF LW_COUNT = 1.
    CONCATENATE LS_EXTRACT-REPID LS_EXTRACT-RITEM INTO LW_NAME.
    LS_header-tdobject   = 'ZRPC'.
    LS_header-tdname     = LW_NAME.
    LS_header-tdid       = 'Z002'.
    LS_header-tdspras    = 'E'.
    LS_header-tdlinesize = 70.

    PERFORM PROC_TEXT
    USING 'U'
          LS_header
*    CHANGING LT_LINES
    .
  ELSE.
    RETURN.
  ENDIF.
ENDFORM.
*}   INSERT

*{   INSERT         DEVK909644                                        2
*&---------------------------------------------------------------------*
*& Form PROC_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM PROC_TEXT
  USING LPW_MODE  TYPE CHAR1
        LPS_HEADR TYPE THEAD.
  DATA: LT_LINES   TYPE TABLE OF TLINE.

  IF LPW_MODE = 'D'. "Display
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        ID                            = LPS_HEADR-tdid
        LANGUAGE                      = LPS_HEADR-tdspras
        NAME                          = LPS_HEADR-TDNAME
        OBJECT                        = LPS_HEADR-tdobject
      TABLES
        LINES                         = LT_LINES
      EXCEPTIONS
        ID                            = 1
        LANGUAGE                      = 2
        NAME                          = 3
        NOT_FOUND                     = 4
        OBJECT                        = 5
        REFERENCE_CHECK               = 6
        WRONG_ACCESS_TO_ARCHIVE       = 7
        OTHERS                        = 8
               .
    IF SY-SUBRC <> 0.

    ENDIF.

  CALL FUNCTION 'EDIT_TEXT'
    EXPORTING
      DISPLAY             = 'X'
      HEADER              = LPS_HEADR
*      SAVE                = 'X'
    TABLES
      LINES               = LT_LINES
    EXCEPTIONS
      ID                  = 1
      LANGUAGE            = 2
      LINESIZE            = 3
      NAME                = 4
      OBJECT              = 5
      TEXTFORMAT          = 6
      COMMUNICATION       = 7
      OTHERS              = 8
               .
    IF SY-SUBRC <> 0.
*     Implement suitable error handling here
    ENDIF.
  ELSE. "update
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        ID                            = LPS_HEADR-tdid
        LANGUAGE                      = LPS_HEADR-tdspras
        NAME                          = LPS_HEADR-TDNAME
        OBJECT                        = LPS_HEADR-tdobject
      TABLES
        LINES                         = LT_LINES
      EXCEPTIONS
        ID                            = 1
        LANGUAGE                      = 2
        NAME                          = 3
        NOT_FOUND                     = 4
        OBJECT                        = 5
        REFERENCE_CHECK               = 6
        WRONG_ACCESS_TO_ARCHIVE       = 7
        OTHERS                        = 8
               .
    IF SY-SUBRC <> 0.
      CALL FUNCTION 'SAVE_TEXT'
        EXPORTING
          HEADER                  = LPS_HEADR
        TABLES
          LINES                   = LT_LINES
        EXCEPTIONS
          ID                      = 1
          LANGUAGE                = 2
          NAME                    = 3
          OBJECT                  = 4
          OTHERS                  = 5
                .
      IF SY-SUBRC <> 0.
*       Implement suitable error handling here
      ENDIF.
    ENDIF.

    CALL FUNCTION 'EDIT_TEXT'
      EXPORTING
*        DISPLAY             = 'X'
        HEADER              = LPS_HEADR
        SAVE                = 'X'
      TABLES
        LINES               = LT_LINES
      EXCEPTIONS
        ID                  = 1
        LANGUAGE            = 2
        LINESIZE            = 3
        NAME                = 4
        OBJECT              = 5
        TEXTFORMAT          = 6
        COMMUNICATION       = 7
        OTHERS              = 8
               .
    IF SY-SUBRC <> 0.
*     Implement suitable error handling here
    ENDIF.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        HEADER                  = LPS_HEADR
      TABLES
        LINES                   = LT_LINES
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        OBJECT                  = 4
        OTHERS                  = 5
              .
    IF SY-SUBRC <> 0.
*     Implement suitable error handling here
    ENDIF.
  ENDIF.
ENDFORM.
*}   INSERT

*{   INSERT         DEVK909644                                        3
*&---------------------------------------------------------------------*
*& Form PROCESS_FC_DISP_DF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM PROCESS_FC_DISP_DF .
  DATA:
        LW_NAME    TYPE THEAD-TDNAME,
        LW_COUNT   TYPE I,
        LW_MODE    TYPE CHAR1,

        LS_EXTRACT LIKE ZVI_BM_RPC_WC_EXTRACT,
        LS_header  TYPE THEAD,

        LT_LINES   TYPE TABLE OF TLINE
        .
  LOOP AT EXTRACT.
    ZVI_BM_RPC_WC_EXTRACT = EXTRACT.
    IF ZVI_BM_RPC_WC_EXTRACT-MARK IS NOT INITIAL..
      LW_COUNT  = LW_COUNT + 1.
      LS_EXTRACT = ZVI_BM_RPC_WC_EXTRACT.
    ENDIF.
  ENDLOOP.
  IF LW_COUNT = 1.
    CONCATENATE LS_EXTRACT-REPID LS_EXTRACT-RITEM INTO LW_NAME.
    LS_header-tdobject   = 'ZRPC'.
    LS_header-tdname     = LW_NAME.
    LS_header-tdid       = 'Z002'.
    LS_header-tdspras    = 'E'.
    LS_header-tdlinesize = 70.

    "Display mode
    PERFORM PROC_TEXT
      USING 'D'
            LS_header
            .
  ELSE.
    RETURN.
  ENDIF.
ENDFORM.
*}   INSERT

*{   INSERT         DEVK909644                                        4
*&---------------------------------------------------------------------*
*& Form PROCESS_FC_EDIT_WF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM PROCESS_FC_EDIT_WF .
  DATA:
        LW_NAME    TYPE THEAD-TDNAME,
        LW_COUNT   TYPE I,
        LW_MODE    TYPE CHAR1,

        LS_EXTRACT LIKE ZVI_BM_RPC_WC_EXTRACT,
        LS_header  TYPE THEAD,

        LT_LINES   TYPE TABLE OF TLINE
        .
  LOOP AT EXTRACT.
    ZVI_BM_RPC_WC_EXTRACT = EXTRACT.
    IF ZVI_BM_RPC_WC_EXTRACT-MARK IS NOT INITIAL..
      LW_COUNT  = LW_COUNT + 1.
      LS_EXTRACT = ZVI_BM_RPC_WC_EXTRACT.
    ENDIF.
  ENDLOOP.
  IF LW_COUNT = 1.
    CONCATENATE LS_EXTRACT-REPID LS_EXTRACT-RITEM LS_EXTRACT-BUKRS INTO LW_NAME.
    LS_header-tdobject   = 'ZRPC'.
    LS_header-tdname     = LW_NAME.
    LS_header-tdid       = 'Z002'.
    LS_header-tdspras    = 'E'.
    LS_header-tdlinesize = 70.

    "update mode
    PERFORM PROC_TEXT
      USING 'U'
            LS_header
            .
  ELSE.
    RETURN.
  ENDIF.
ENDFORM.
*}   INSERT
