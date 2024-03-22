*----------------------------------------------------------------------*
***INCLUDE LZFG_BM_RPCF01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  SEGMENT_ITEM_SET_TEMPL_SHOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LPW_RPC_SI_TEMPL  text
*      <--LPS_BM_RPC_SI  text
*----------------------------------------------------------------------*
FORM SEGMENT_ITEM_SET_TEMPL_SHOW
  USING    LPW_TEMPL          TYPE ZDD_BM_RPC_TEMPL
  CHANGING LPS_BM_RPC_SI      TYPE ZST1_BM_RPC_SI.
  DATA:
    LW_SHOWFIELD              TYPE FIELDNAME.

  LW_SHOWFIELD = 'SHOW' && LPW_TEMPL.
  ASSIGN COMPONENT LW_SHOWFIELD OF STRUCTURE LPS_BM_RPC_SI
    TO FIELD-SYMBOL(<LF_SHOW>).
  IF SY-SUBRC IS INITIAL.
    <LF_SHOW> = 'X'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SEGMENT_ITEM_SET_TEMPL_SHOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LPW_RPC_SI_TEMPL  text
*      -->LPS_RPC_D  text
*      <--LPS_BM_RPC_SI  text
*----------------------------------------------------------------------*
FORM SEGMENT_ITEM_SET_TEMPL_DATA
  USING    LPW_TEMPL          TYPE ZDD_BM_RPC_TEMPL
           LPS_RPC_D          TYPE ZTB_BM_RPC_D
  CHANGING LPS_BM_RPC_SI      TYPE ZST1_BM_RPC_SI.
  DATA:
    LW_TEMPLFIELD TYPE FIELDNAME,
    LS_DATA_EXC   TYPE ZST0_BM_RPC_D.
  FIELD-SYMBOLS:
    <LF_TEMPL_DATA>           TYPE ZST1_BM_RPC_SI_TEMPL.

  LW_TEMPLFIELD = 'TEMPL' && LPW_TEMPL.
  ASSIGN COMPONENT LW_TEMPLFIELD OF STRUCTURE LPS_BM_RPC_SI
    TO <LF_TEMPL_DATA>.
  IF SY-SUBRC IS INITIAL.
    MOVE-CORRESPONDING LPS_RPC_D TO LS_DATA_EXC.
    APPEND LS_DATA_EXC TO <LF_TEMPL_DATA>-DATA.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SEGMENT_ITEM_SET_TEMPL_FSV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LPW_RPC_SI_TEMPL  text
*      -->LPT_FSV_ITEMS  text
*      <--LPS_BM_RPC_SI  text
*----------------------------------------------------------------------*
FORM SEGMENT_ITEM_SET_TEMPL_FSV
  USING    LPS_RPC_SI         TYPE ZST0_BM_RPC_SI
           LPT_FSV_ITEMS      TYPE ZTT_BM_RPC_FSV_D
  CHANGING LPS_BM_RPC_SI      TYPE ZST1_BM_RPC_SI.
  DATA:
    LW_TEMPLFIELD TYPE FIELDNAME,
    LW_BOLDLV     TYPE I,
    LS_DATA_EXC   TYPE ZST0_BM_RPC_D,
    LT_DATA_EXC   TYPE TABLE OF ZST0_BM_RPC_D.
  FIELD-SYMBOLS:
    <LF_TEMPL_DATA>           TYPE ZST1_BM_RPC_SI_TEMPL.

  IF LPS_RPC_SI-VERSN = 'ZBS1'.
    LW_BOLDLV = 4.
  ELSE.
    LW_BOLDLV = 2.
  ENDIF.
  LW_TEMPLFIELD = 'TEMPL' && LPS_RPC_SI-TEMPL.
  ASSIGN COMPONENT LW_TEMPLFIELD OF STRUCTURE LPS_BM_RPC_SI
    TO <LF_TEMPL_DATA>.
  IF SY-SUBRC IS INITIAL.
    LOOP AT LPS_RPC_SI-ROWS INTO DATA(LS_SIR).
      CLEAR: LS_DATA_EXC.
      IF GS_RPC_R-LANGU = 'E'.
        LS_DATA_EXC-CEL01-CELLVAL = LS_SIR-COL01.
        LS_DATA_EXC-CEL02-CELLVAL = LS_SIR-COL02.
        LS_DATA_EXC-CEL03-CELLVAL = LS_SIR-COL03.
        LS_DATA_EXC-CEL04-CELLVAL = LS_SIR-COL04.
        LS_DATA_EXC-CEL05-CELLVAL = LS_SIR-COL05.
      ELSE.
        LS_DATA_EXC-CEL01-CELLVAL = LS_SIR-VIDATA-COL01.
        LS_DATA_EXC-CEL02-CELLVAL = LS_SIR-VIDATA-COL02.
        LS_DATA_EXC-CEL03-CELLVAL = LS_SIR-VIDATA-COL03.
        LS_DATA_EXC-CEL04-CELLVAL = LS_SIR-VIDATA-COL04.
        LS_DATA_EXC-CEL05-CELLVAL = LS_SIR-VIDATA-COL05.
      ENDIF.
      LS_DATA_EXC-CEL01-CSTLD   = LS_DATA_EXC-CEL02-CSTLD
      = LS_DATA_EXC-CEL03-CSTLD = LS_DATA_EXC-CEL04-CSTLD
      = LS_DATA_EXC-CEL05-CSTLD = 'X'.
      APPEND LS_DATA_EXC TO <LF_TEMPL_DATA>-DATA.
    ENDLOOP.

    LOOP AT LPT_FSV_ITEMS INTO DATA(LS_FSV_ITEM).
      CASE LPS_RPC_SI-TEMPL.
        WHEN GC_TEMPL_FSV_4.
          PERFORM GEN_FSV_LINE_4_COL
            USING LS_FSV_ITEM
                  LW_BOLDLV
            CHANGING LS_DATA_EXC.
        WHEN GC_TEMPL_FSV_5.
          PERFORM GEN_FSV_LINE_5_COL
            USING LS_FSV_ITEM
                  LW_BOLDLV
            CHANGING LS_DATA_EXC.
        WHEN OTHERS.
      ENDCASE.

      APPEND LS_DATA_EXC TO <LF_TEMPL_DATA>-DATA.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SEGMENT_ITEM_SET_TEMPL_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LPW_RPC_SI_TEMPL  text
*      -->LPS_RPC_D  text
*      <--LPS_BM_RPC_SI  text
*----------------------------------------------------------------------*
FORM SEGMENT_ITEM_SET_TEMPL_TABLE
  USING    LPS_RPC_F          TYPE ZST0_BM_RPC_F
           LPW_LANGU          TYPE LANGU
  CHANGING LPS_BM_RPC_SI      TYPE ZST1_BM_RPC_SI.
  DATA:
    LW_FIELDNAME  TYPE FIELDNAME,
    LW_COLNO      TYPE ZDD_BM_RPC_COLNO,
    LW_TEMPLFIELD TYPE FIELDNAME,
    LS_CELL_OUT   TYPE ZST0_BM_RPC_CELLOUTPUT,
    LS_CSTYLE     TYPE ZDD_BM_RPC_CSTYLE,
    LW_POS        TYPE I,
    LS_RPC_D      TYPE ZST0_BM_RPC_D.
  FIELD-SYMBOLS:
    <LF_TEMPL_DATA>           TYPE ZST1_BM_RPC_SI_TEMPL.

  LW_TEMPLFIELD = 'TEMPL' && LPS_RPC_F-SEGITM-TEMPL.
  ASSIGN COMPONENT LW_TEMPLFIELD OF STRUCTURE LPS_BM_RPC_SI
    TO <LF_TEMPL_DATA>.
  IF SY-SUBRC IS INITIAL.
*   Set header rows data
    LOOP AT LPS_RPC_F-SEGITM-ROWS INTO DATA(LS_SIR)
      WHERE HROW IS NOT INITIAL.
      CLEAR: LS_RPC_D.
      MOVE-CORRESPONDING LS_SIR TO LS_RPC_D.
      IF LPW_LANGU <> 'E'.
        MOVE-CORRESPONDING LS_SIR-VIDATA TO LS_RPC_D.
      ENDIF.

      DO GC_MAXCOL TIMES.
        LW_COLNO = SY-INDEX.
        LW_FIELDNAME = 'COL' && LW_COLNO.
        ASSIGN COMPONENT LW_FIELDNAME OF STRUCTURE LS_RPC_D
          TO FIELD-SYMBOL(<LF_CELL>).
        IF SY-SUBRC IS INITIAL.
          CLEAR: LS_CELL_OUT.
          LS_CELL_OUT-CELLVAL = <LF_CELL>.
          PERFORM CELL_CONVERT_VARIABLE
            USING LPS_RPC_F
                  LS_CELL_OUT-CELLVAL.

          LW_POS = LW_COLNO - 1.
          LS_CSTYLE = LS_SIR-LSTYLE+LW_POS(1).
          PERFORM CONVERT_CELL_STYLE_TO_OUTPUT
            USING LS_CSTYLE
                  LS_CELL_OUT.

          LW_FIELDNAME = 'CEL' && LW_COLNO.
          ASSIGN COMPONENT LW_FIELDNAME OF STRUCTURE LS_RPC_D
            TO FIELD-SYMBOL(<LF_CELLOUT>).
          IF SY-SUBRC IS INITIAL.
            <LF_CELLOUT> = LS_CELL_OUT.
          ENDIF.
          IF LS_CSTYLE IS NOT INITIAL.
            APPEND LS_CELL_OUT TO LS_RPC_D-CELLS.
          ENDIF.
        ENDIF.
      ENDDO.
      CASE LS_SIR-HROW.
        WHEN 1.
          IF LPS_RPC_F-SEGITM-NOLEAD > 1.
            APPEND LS_RPC_D TO <LF_TEMPL_DATA>-HEADROW1.
          ELSE.
            APPEND LS_RPC_D TO <LF_TEMPL_DATA>-DATA.
          ENDIF.
        WHEN 2.
          APPEND LS_RPC_D TO <LF_TEMPL_DATA>-HEADROW2.
        WHEN 3.
          APPEND LS_RPC_D TO <LF_TEMPL_DATA>-HEADROW3.
      ENDCASE.
    ENDLOOP.

    LOOP AT LPS_RPC_F-DATA ASSIGNING FIELD-SYMBOL(<LF_RPC_D>).
      DO GC_MAXCOL TIMES.
        LW_COLNO = SY-INDEX.
        LW_FIELDNAME = 'COL' && LW_COLNO.
        ASSIGN COMPONENT LW_FIELDNAME OF STRUCTURE <LF_RPC_D>
          TO <LF_CELL>.
        IF SY-SUBRC IS INITIAL.
          CLEAR: LS_CELL_OUT.
          LS_CELL_OUT-CELLVAL = <LF_CELL>.

          READ TABLE LPS_RPC_F-SEGITM-ROWS INTO LS_SIR
            WITH KEY ROWNO = <LF_RPC_D>-ROWPOS
                     HROW  = SPACE.
          IF SY-SUBRC IS INITIAL.
            LW_POS = LW_COLNO - 1.
            LS_CSTYLE = LS_SIR-LSTYLE+LW_POS(1).
            PERFORM CONVERT_CELL_STYLE_TO_OUTPUT
              USING LS_CSTYLE
                    LS_CELL_OUT.
          ENDIF.

          LW_FIELDNAME = 'CEL' && LW_COLNO.
          ASSIGN COMPONENT LW_FIELDNAME OF STRUCTURE <LF_RPC_D>
            TO <LF_CELLOUT>.
          IF SY-SUBRC IS INITIAL.
            <LF_CELLOUT> = LS_CELL_OUT.
          ENDIF.
          IF LS_CSTYLE IS NOT INITIAL.
            APPEND LS_CELL_OUT TO <LF_RPC_D>-CELLS.
          ENDIF.
        ENDIF.
      ENDDO.

    ENDLOOP.
    APPEND LINES OF LPS_RPC_F-DATA TO <LF_TEMPL_DATA>-DATA.
  ENDIF.

  IF LPS_RPC_F-SEGITM-NOLEAD > 1.
    PERFORM FORM_COVERT_DATA_TO_LEADDATA
      USING LPS_RPC_F
      CHANGING <LF_TEMPL_DATA>.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SEGMENT_ITEM_SET_TEMPL_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LPW_RPC_SI_TEMPL  text
*      -->LPS_RPC_D  text
*      <--LPS_BM_RPC_SI  text
*----------------------------------------------------------------------*
FORM SEGMENT_ITEM_SET_TEMPL_TEXT
  USING    LPS_RPC_R          TYPE ZST0_BM_RPC_R
           LPS_RPC_F          TYPE ZST0_BM_RPC_F
  CHANGING LPS_BM_RPC_SI      TYPE ZST1_BM_RPC_SI.
  DATA:
    LW_FIELDNAME  TYPE FIELDNAME,
    LW_ROWNO      TYPE ZDD_BM_RPC_ROWNO,
    LW_TEMPLFIELD TYPE FIELDNAME,
    LS_CELL_OUT   TYPE ZST0_BM_RPC_CELLOUTPUT,
    LS_CSTYLE     TYPE ZDD_BM_RPC_CSTYLE,
    LS_TDTEXT     TYPE EFG_STRN_TDTEXT,
    LT_STREAM     TYPE TABLE OF TEXT132,
    LS_RPC_TEXT   TYPE ZST_BM_RPC_TEXT.
  FIELD-SYMBOLS:
    <LF_TEMPL_DATA>           TYPE ZST1_BM_RPC_SI_TEXT.

  LW_TEMPLFIELD = 'TEMPL' && LPS_RPC_F-SEGITM-TEMPL.
  ASSIGN COMPONENT LW_TEMPLFIELD OF STRUCTURE LPS_BM_RPC_SI
    TO <LF_TEMPL_DATA>.
  IF SY-SUBRC IS INITIAL.
    LOOP AT LPS_RPC_F-DATA ASSIGNING FIELD-SYMBOL(<LF_RPC_D>).
      CLEAR: LS_RPC_TEXT.

      LW_ROWNO = <LF_RPC_D>-ROWPOS.
      LS_TDTEXT-TDNAME = LPS_RPC_R-REPID && LPS_RPC_F-EFMON
                      && LPS_RPC_F-RPSEG  && LPS_RPC_F-RITEM
                      && LW_ROWNO.

      LS_TDTEXT-TDSPRAS = LPS_RPC_R-LANGU.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          ID                      = 'Z001'
          LANGUAGE                = LS_TDTEXT-TDSPRAS
          NAME                    = LS_TDTEXT-TDNAME
          OBJECT                  = 'ZRPC'
        IMPORTING
          HEADER                  = LS_TDTEXT-THEAD
        TABLES
          LINES                   = LS_TDTEXT-T_LINES
        EXCEPTIONS
          ID                      = 1
          LANGUAGE                = 2
          NAME                    = 3
          NOT_FOUND               = 4
          OBJECT                  = 5
          REFERENCE_CHECK         = 6
          WRONG_ACCESS_TO_ARCHIVE = 7
          OTHERS                  = 8.
      CHECK SY-SUBRC IS INITIAL.
      CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
        EXPORTING
          LANGUAGE    = LS_TDTEXT-TDSPRAS
        TABLES
          ITF_TEXT    = LS_TDTEXT-T_LINES
          TEXT_STREAM = LT_STREAM.
      CONCATENATE LINES OF LT_STREAM INTO LS_RPC_TEXT-CELLVAL.
*      LS_RPC_TEXT-CSTLB = 'X'.
*      LS_RPC_TEXT-CSTLI = 'X'.
      LS_RPC_TEXT-CSTLN = 'X'.
*      LS_RPC_TEXT-CSTLU = 'X'.
*      LS_RPC_TEXT-CSTLD = 'X'.
      APPEND LS_RPC_TEXT TO <LF_TEMPL_DATA>-DATA.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PREPARE_OUTPUT_SI_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PREPARE_OUTPUT_SI_TABLE.
  DATA:
    LS_COL TYPE ZST0_BM_RPC_SIC.

*  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
*    EXPORTING
*      I_STRUCTURE_NAME   = 'ZST0_BM_RPC_SICELL'
*      I_INTERNAL_TABNAME = 'ZST0_BM_RPC_SICELL'
*    CHANGING
*      CT_FIELDCAT        = GS_RPC_DS_SI-FCAT.

* Set column labels
  LOOP AT GS_RPC_DS_SI-FCAT ASSIGNING FIELD-SYMBOL(<LF_FCAT>).
    READ TABLE GS_RPC_DS_SI-COLUMNS INTO LS_COL
      WITH KEY  FIELDNAME = <LF_FCAT>-FIELDNAME.
    IF SY-SUBRC IS INITIAL.
      <LF_FCAT>-NO_OUT = SPACE.
*     Set leading color
      IF LS_COL-LEADCOL = 'X'.
        IF <LF_FCAT>-FIELDNAME = 'DESCR'.
          <LF_FCAT>-EMPHASIZE = GC_COLOR_DESCR.
        ELSE.
          <LF_FCAT>-EMPHASIZE = GC_COLOR_LDCOL.
        ENDIF.
      ELSEIF LS_COL-COLNO > GS_RPC_DS_SI-NOCOL.
        <LF_FCAT>-EMPHASIZE = GC_COLOR_DESCR.
      ELSE.
        CLEAR: <LF_FCAT>-EMPHASIZE.
      ENDIF.

*     Set heading label
      IF LS_COL-COLNO IS INITIAL.
        <LF_FCAT>-OUTPUTLEN = '30'.
      ELSE.
        <LF_FCAT>-SCRTEXT_L = <LF_FCAT>-SCRTEXT_M = <LF_FCAT>-SCRTEXT_S
                            = <LF_FCAT>-COLTEXT   = 'Y0' && LS_COL-COLNO.
        <LF_FCAT>-OUTPUTLEN = GC_COL_OUTLENGTH.
      ENDIF.
    ELSE.
      <LF_FCAT>-NO_OUT = 'X'.
    ENDIF.
  ENDLOOP.

  PERFORM SI_GEN_DISPLAY_CELLS.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PREPARE_OUTPUT_FORM_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PREPARE_OUTPUT_FORM_DATA.
  DATA:
    LS_COL TYPE ZST0_BM_RPC_SIC.

* Set column labels
  LOOP AT GS_RPC_IN_F-FCAT ASSIGNING FIELD-SYMBOL(<LF_FCAT>).
    READ TABLE GS_RPC_IN_F-SEGITM-COLUMNS INTO LS_COL
      WITH KEY  FIELDNAME = <LF_FCAT>-FIELDNAME.
    IF SY-SUBRC IS INITIAL.
      <LF_FCAT>-NO_OUT = SPACE.
*     Set leading color
      IF LS_COL-LEADCOL = 'X'.
        IF <LF_FCAT>-FIELDNAME = 'DESCR'.
          <LF_FCAT>-EMPHASIZE = GC_COLOR_DESCR.
        ELSE.
          <LF_FCAT>-EMPHASIZE = GC_COLOR_LDCOL.
        ENDIF.
      ENDIF.

*     Set heading label
      IF LS_COL-COLNO IS INITIAL.
        <LF_FCAT>-OUTPUTLEN = '30'.
      ELSE.
        <LF_FCAT>-SCRTEXT_L = <LF_FCAT>-SCRTEXT_M = <LF_FCAT>-SCRTEXT_S
                            = <LF_FCAT>-COLTEXT   = '#' && LS_COL-COLNO.
        <LF_FCAT>-OUTPUTLEN = GC_COL_OUTLENGTH.
      ENDIF.

      IF LS_COL-FIGTY = GC_FIGTY_INPUT.
        <LF_FCAT>-EDIT = 'X'.
      ELSE.
        <LF_FCAT>-EDIT = SPACE.
      ENDIF.
    ELSE.
      IF GS_RPC_IN_F-SEGITM-ITMTY = GC_ITEMTYPE_TEXT.
        IF <LF_FCAT>-FIELDNAME = 'ROWNO'.
          <LF_FCAT>-NO_OUT = SPACE.
          <LF_FCAT>-OUTPUTLEN = '30'.
*          <LF_FCAT>-EMPHASIZE = GC_COLOR_DESCR.
        ELSEIF <LF_FCAT>-FIELDNAME = 'TEX01' OR <LF_FCAT>-FIELDNAME = 'TEX02' .
          <LF_FCAT>-NO_OUT = SPACE.
          <LF_FCAT>-OUTPUTLEN = '30'.
          <LF_FCAT>-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_HOTSPOT.
        ELSE.
          <LF_FCAT>-NO_OUT = 'X'.
        ENDIF.
      ELSE.
        <LF_FCAT>-NO_OUT = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM FORM_GEN_DISPLAY_CELLS.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  INIT_SI_CELLS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM INIT_SI_COLS_ROWS.
  DATA:
    LS_COL TYPE ZST0_BM_RPC_SIC,
    LS_SIR TYPE ZST0_BM_RPC_SIR.

  CHECK GS_RPC_DS_SI-COLUMNS IS INITIAL
    AND GS_RPC_DS_SI-ROWS IS INITIAL.

* Generate columns
  LOOP AT GS_RPC_DS_SI-FCAT ASSIGNING FIELD-SYMBOL(<LF_FCAT>).
    CLEAR: LS_COL.
    LS_COL-FIELDNAME = <LF_FCAT>-FIELDNAME.
    LS_COL-SRCTAB    = 'ACDOCA'.

    IF <LF_FCAT>-FIELDNAME CS 'COL'.
      LS_COL-COLNO = <LF_FCAT>-FIELDNAME+3.
      IF LS_COL-COLNO <= GS_RPC_DS_SI-NOLEAD.
        LS_COL-LEADCOL = 'X'.
      ENDIF.

      IF LS_COL-COLNO > GS_RPC_DS_SI-NOCOL.
        <LF_FCAT>-NO_OUT = 'X'.
      ELSE.
        APPEND LS_COL TO GS_RPC_DS_SI-COLUMNS.
      ENDIF.
    ELSEIF <LF_FCAT>-FIELDNAME = 'DESCR'.
      LS_COL-LEADCOL = 'X'.
      APPEND LS_COL TO GS_RPC_DS_SI-COLUMNS.
    ELSE.
      <LF_FCAT>-NO_OUT = 'X'.
    ENDIF.
  ENDLOOP.

* Add header rows info
  DO GS_RPC_DS_SI-HROWS TIMES.
    CLEAR: LS_SIR.
    LS_SIR-ROWNO  = SY-INDEX.
    LS_SIR-HROW   = SY-INDEX.
    CONCATENATE TEXT-R01 LS_SIR-ROWNO INTO LS_SIR-DESCR
      SEPARATED BY SPACE.
    APPEND LS_SIR TO GS_RPC_DS_SI-ROWS.
  ENDDO.

* Add normal rows info
  DO GS_RPC_DS_SI-NOROWS TIMES.
    CLEAR: LS_SIR.
    LS_SIR-ROWNO  = SY-INDEX.
    LS_SIR-SRCTAB = 'ACDOCA'.
    CONCATENATE TEXT-R02 LS_SIR-ROWNO INTO LS_SIR-DESCR.
    APPEND LS_SIR TO GS_RPC_DS_SI-ROWS.
  ENDDO.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0100_PBO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0100_PBO .
  DATA:
    LS_LAYOUT TYPE LVC_S_LAYO,
    LO_SI_ALV TYPE REF TO CL_GUI_ALV_GRID.

*  LS_LAYOUT-STYLEFNAME = 'STYLEFNAME'.
  LS_LAYOUT-NO_TOOLBAR = 'X'.
  LS_LAYOUT-INFO_FNAME = 'INFO_FNAME'.
  LS_LAYOUT-CTAB_FNAME = 'CTAB_FNAME'.
  LS_LAYOUT-SEL_MODE = 'D'.

  IF GO_SI_ALV IS INITIAL.
    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR'
      EXPORTING
        I_CPROG            = SY-REPID
        I_DYNNR            = SY-DYNNR
        I_CUS_CONTROL_NAME = 'CUS_ALV_SI'
        IS_LAYOUT          = LS_LAYOUT
        I_CUSTOMIZE_ALV    = 'X'
      IMPORTING
        E_ALV_GRID_MERGE   = GO_SI_ALV
        E_CUS_CONTAINER    = GO_SI_CON
      CHANGING
        IT_OUTTAB          = GS_RPC_DS_SI-CELLS
        IT_FIELDCATALOG    = GS_RPC_DS_SI-FCAT.

*   Creating an instance for the event handler
    CREATE OBJECT GO_EVENT_HANDLER .
    SET HANDLER GO_EVENT_HANDLER->HANDLE_DOUBLE_CLICK FOR GO_SI_ALV .
  ELSE.
    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
      EXPORTING
        IT_FIELDCAT = GS_RPC_DS_SI-FCAT
        I_ALV_GRID  = GO_SI_ALV.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0100_ADD_ROWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0100_ADD_ROWS .
  DATA:
    LS_SIR      TYPE ZST0_BM_RPC_SIR,
    LS_SIR_LAST TYPE ZST0_BM_RPC_SIR.

  LOOP AT GS_RPC_DS_SI-ROWS INTO LS_SIR_LAST
    WHERE HROW IS INITIAL.
  ENDLOOP.

  CLEAR: LS_SIR.
  LS_SIR-ROWNO  = LS_SIR_LAST-ROWNO + 1.
  CONCATENATE TEXT-R02 LS_SIR-ROWNO INTO LS_SIR-DESCR.
  LS_SIR-SRCTAB  = 'ACDOCA'.
  APPEND LS_SIR TO GS_RPC_DS_SI-ROWS.

  PERFORM SI_GEN_DISPLAY_CELLS.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0100_ADD_HEADER_ROWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0100_ADD_HEADER_ROWS .
  DATA:
    LS_SIR      TYPE ZST0_BM_RPC_SIR,
    LS_SIR_LAST TYPE ZST0_BM_RPC_SIR,
    LW_ANSWER   TYPE CHAR1,
    LW_INDEX    TYPE I.

  CLEAR: LS_SIR.

  IF GS_RPC_DS_SI-HROWS > 1.
    CALL FUNCTION 'ZFM_POPUP_SET_DATA_RECORD'
      EXPORTING
        I_POPUP_TITLE  = TEXT-006
        I_SUB_TABNAME  = 'ZTB_BM_RPC_SIR' "'ZST0_BM_RPC_SIR'
        I_SUB_FNAME    = 'HROW' "LS_SIC-FIELDNAME
        I_SUB_REQUIRED = 'X'
      IMPORTING
        RETURNCODE     = LW_ANSWER
      CHANGING
        C_RECORD       = LS_SIR.
    CHECK LW_ANSWER IS INITIAL.
  ELSE.
    LS_SIR-HROW = 1.
  ENDIF.

  LW_INDEX = 1.
  LOOP AT GS_RPC_DS_SI-ROWS INTO LS_SIR_LAST
    WHERE HROW = LS_SIR-HROW AND HROW IS NOT INITIAL.
    LW_INDEX = SY-TABIX.
  ENDLOOP.

  LS_SIR-ROWNO  = LS_SIR_LAST-ROWNO + 1.
  CONCATENATE TEXT-R01 LS_SIR-HROW TEXT-R03 LS_SIR-ROWNO INTO LS_SIR-DESCR
    SEPARATED BY SPACE.

  APPEND LS_SIR TO GS_RPC_DS_SI-ROWS.

  PERFORM SI_GEN_DISPLAY_CELLS.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SI_GEN_DISPLAY_CELLS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SI_GEN_DISPLAY_CELLS .
  DATA:
    LS_LINE_CELL TYPE ZST0_BM_RPC_SICELL,
    LW_ROWNO     TYPE ZST0_BM_RPC_SIR-ROWNO,
    LW_ROWIX     TYPE I,
    LW_COLNO     TYPE ZST0_BM_RPC_SIC-COLNO,
    LS_STYLE     TYPE ZST_BM_LVC_STYL.

  SORT GS_RPC_DS_SI-ROWS BY HROW DESCENDING ROWNO.

  CLEAR: GS_RPC_DS_SI-CELLS.
  LOOP AT GS_RPC_DS_SI-ROWS INTO DATA(LS_SIR).
    CLEAR: LS_LINE_CELL, LW_COLNO, LS_STYLE.
    LW_ROWIX = SY-TABIX.

    MOVE-CORRESPONDING LS_SIR TO LS_LINE_CELL.

    LW_COLNO = 1.
    READ TABLE GS_RPC_DS_SI-COLUMNS INTO DATA(LS_SIC)
      WITH KEY LEADCOL = SPACE.
    IF SY-SUBRC IS INITIAL.
      LW_COLNO = LS_SIC-COLNO.
    ENDIF.

*   Merge header row
    IF LS_SIR-HROW IS NOT INITIAL.
      LS_LINE_CELL-INFO_FNAME = GC_COLOR_HROW.
      LS_STYLE-STYLE = ALV_STYLE_ALIGN_CENTER_CENTER
                     + ALV_STYLE_FONT_BOLD
                     + ALV_STYLE_FONT_UNDERLINED.
      IF LS_SIR-HROW = GC_HROW_2_CELLS.
        LS_STYLE-MERGEHORIZ = 2.
        WHILE LW_COLNO < GS_RPC_DS_SI-NOCOL.
          LS_STYLE-FIELDNAME = 'COL' && LW_COLNO.
          LW_COLNO = LW_COLNO + LS_STYLE-MERGEHORIZ.
          APPEND LS_STYLE TO LS_LINE_CELL-ZSTYLE.
        ENDWHILE.

      ELSEIF LS_SIR-HROW = GC_HROW_4_CELLS.
        LS_STYLE-MERGEHORIZ = 4.
        WHILE LW_COLNO < GS_RPC_DS_SI-NOCOL.
          LS_STYLE-FIELDNAME = 'COL' && LW_COLNO.
          LW_COLNO = LW_COLNO + LS_STYLE-MERGEHORIZ.
          APPEND LS_STYLE TO LS_LINE_CELL-ZSTYLE.
        ENDWHILE.
      ENDIF.

*   Merge detail not leading
    ELSEIF LW_ROWNO IS INITIAL.
      LW_ROWNO = LW_ROWIX.
      CLEAR: LS_STYLE.

      IF LS_SIC-FIELDNAME IS NOT INITIAL.
        LS_STYLE-FIELDNAME = LS_SIC-FIELDNAME.
        LS_STYLE-MERGEHORIZ = LINES( GS_RPC_DS_SI-COLUMNS ) - LS_SIC-COLNO.
        LS_STYLE-MERGEHORIZ = GS_RPC_DS_SI-NOCOL + 1  - LS_SIC-COLNO.
        LS_STYLE-MERGEVERT  = LINES( GS_RPC_DS_SI-ROWS ) - LW_ROWNO + 1.
        APPEND LS_STYLE TO LS_LINE_CELL-ZSTYLE.
      ENDIF.
    ENDIF.
    APPEND LS_LINE_CELL TO GS_RPC_DS_SI-CELLS.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SI_GEN_DISPLAY_CELLS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FORM_GEN_DISPLAY_CELLS .
  IF GS_RPC_IN_F-SEGITM-ITMTY = GC_ITEMTYPE_TABLE.
    PERFORM FORM_GEN_DISPLAY_CELLS_TABLE.
  ELSE.
    PERFORM FORM_GEN_DISPLAY_CELLS_TEXT.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FORM_GEN_DISPLAY_CELLS_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FORM_GEN_DISPLAY_CELLS_TABLE .
  DATA:
    LS_LINE_CELL TYPE ZST0_BM_RPC_SICELL,
    LS_STYLE     TYPE LVC_S_STYL,
    LS_DATA      TYPE ZST0_BM_RPC_D.
  FIELD-SYMBOLS:
    <LF_VALUEROW> TYPE ANY,
    <LF_VALUEDB>  TYPE ANY,
    <LF_VALUE>    TYPE ANY.

  SORT GS_RPC_IN_F-SEGITM-ROWS BY HROW DESCENDING ROWNO.

  CLEAR: GS_RPC_IN_F-CELLS.
  LOOP AT GS_RPC_IN_F-SEGITM-ROWS INTO DATA(LS_SIR).
    CLEAR: LS_LINE_CELL, LS_STYLE, LS_DATA.

    MOVE-CORRESPONDING LS_SIR TO LS_LINE_CELL.
    IF ZST0_BM_RPC_DIMSET-SPRAS = GC_LANGU_VN.
      MOVE-CORRESPONDING LS_SIR-VIDATA TO LS_LINE_CELL.
    ENDIF.
    IF LS_SIR-HROW IS NOT INITIAL.
      LS_LINE_CELL-INFO_FNAME = GC_COLOR_HROW.
      LOOP AT GS_RPC_IN_F-SEGITM-COLUMNS INTO DATA(LS_COL).
        LS_STYLE-FIELDNAME = LS_COL-FIELDNAME.
        LS_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        INSERT LS_STYLE INTO TABLE LS_LINE_CELL-STYLEFNAME.
      ENDLOOP.
    ELSE.
      READ TABLE GS_RPC_IN_F-DATA INTO LS_DATA
        WITH KEY ROWPOS = LS_SIR-ROWNO BINARY SEARCH.

      LOOP AT GS_RPC_IN_F-SEGITM-COLUMNS INTO LS_COL.
        UNASSIGN: <LF_VALUE>.
        ASSIGN COMPONENT LS_COL-FIELDNAME OF STRUCTURE LS_LINE_CELL
          TO <LF_VALUE>.
        ASSIGN COMPONENT LS_COL-FIELDNAME OF STRUCTURE LS_DATA
          TO <LF_VALUEDB>.

        IF LS_SIR-FIGTY = GC_FIGTY_INPUT AND LS_COL-FIGTY <> GC_FIGTY_KFWCP
            AND LS_COL-LEADCOL IS INITIAL AND LS_COL-FIGTY <> GC_FIGTY_FORMULA
        OR LS_COL-FIGTY = GC_FIGTY_INPUT AND LS_SIR-FIGTY <> GC_FIGTY_KFWCP
            AND LS_SIR-FIGTY <> GC_FIGTY_FORMULA.
          LS_STYLE-FIELDNAME = LS_COL-FIELDNAME.
          LS_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
          INSERT LS_STYLE INTO TABLE LS_LINE_CELL-STYLEFNAME.

          IF <LF_VALUEDB> IS NOT INITIAL AND <LF_VALUE> IS ASSIGNED.
            <LF_VALUE> = <LF_VALUEDB>.
          ENDIF.
        ELSE.
          IF LS_COL-LEADCOL IS INITIAL.
            IF <LF_VALUE> IS ASSIGNED.
              IF <LF_VALUEDB> IS INITIAL.
                <LF_VALUE> = TEXT-005.
              ELSE.
                <LF_VALUE> = <LF_VALUEDB>.
              ENDIF.
            ENDIF.
*          ELSEIF <LF_VALUEDB> IS ASSIGNED AND <LF_VALUEDB> IS NOT INITIAL.
*            <LF_VALUE> = <LF_VALUEDB>.
          ENDIF.
          LS_STYLE-FIELDNAME = LS_COL-FIELDNAME.
          LS_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
          INSERT LS_STYLE INTO TABLE LS_LINE_CELL-STYLEFNAME.
        ENDIF.
      ENDLOOP.
    ENDIF.
    APPEND LS_LINE_CELL TO GS_RPC_IN_F-CELLS.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FORM_GEN_DISPLAY_CELLS_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FORM_GEN_DISPLAY_CELLS_TEXT.
  DATA:
    LS_LINE_CELL TYPE ZST0_BM_RPC_SICELL,
    LS_STYLE     TYPE LVC_S_STYL,
    LS_DATA      TYPE ZST0_BM_RPC_D.
  FIELD-SYMBOLS:
    <LF_VALUEROW> TYPE ANY,
    <LF_VALUEDB>  TYPE ANY,
    <LF_VALUE>    TYPE ANY.

  SORT GS_RPC_IN_F-SEGITM-ROWS BY HROW DESCENDING ROWNO.

  CLEAR: GS_RPC_IN_F-CELLS.
  LOOP AT GS_RPC_IN_F-DATA INTO LS_DATA.
    CLEAR: LS_LINE_CELL.
    MOVE-CORRESPONDING LS_DATA TO LS_LINE_CELL.
    LS_LINE_CELL-ROWNO = LS_DATA-ROWPOS.
    APPEND LS_LINE_CELL TO GS_RPC_IN_F-CELLS.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&       Class (Implementation)  LCL_EVENT_HANDLER
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
*Handle Double Click
  METHOD HANDLE_DOUBLE_CLICK .
    PERFORM HANDLE_SI_ALV_DOUBLE_CLICK USING E_ROW E_COLUMN .
  ENDMETHOD .

ENDCLASS.               "LCL_EVENT_HANDLER

*&---------------------------------------------------------------------*
*&      Form  HANDLE_SI_ALV_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->E_ROW  text
*      -->E_COLUMN  text
*      -->ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM HANDLE_SI_ALV_DOUBLE_CLICK
  USING E_ROW     TYPE  LVC_S_ROW
        E_COLUMN  TYPE  LVC_S_COL.
  DATA:
    LT_SICOND    TYPE TABLE OF ZST0_BM_RPC_SICOND,
    LS_SELOPT    TYPE ZST0_BM_RPC_SELOPT,
    LW_FIELDNAME TYPE CHAR61,
    LW_SCRTEXT_L TYPE SCRTEXT_L.

  READ TABLE GS_RPC_DS_SI-COLUMNS INTO DATA(LS_SIC)
    WITH KEY FIELDNAME = E_COLUMN-FIELDNAME.
  CHECK SY-SUBRC IS INITIAL.

  READ TABLE GS_RPC_DS_SI-ROWS ASSIGNING FIELD-SYMBOL(<LF_SIR>)
    INDEX E_ROW-INDEX.
  IF SY-SUBRC IS INITIAL.
*   Select normal row
    IF <LF_SIR>-HROW IS INITIAL.
*     Show Row dimension setting
      IF LS_SIC-FIELDNAME = 'DESCR'.
        PERFORM SITAB_SHOW_ROW_SETTING USING <LF_SIR>.

*     Leading column -> Input label in detail row
      ELSEIF LS_SIC-LEADCOL IS NOT INITIAL.
        PERFORM SITAB_SHOW_LEADING_CELL
          USING LS_SIC
          CHANGING <LF_SIR>.
      ELSE.
*       Normal cell -> Do nothing
      ENDIF.

*   Select header row
    ELSE.
      IF LS_SIC-FIELDNAME = 'DESCR'.
*       Desc header -> Do nothing
        PERFORM SITAB_SHOW_HROW_SETTING USING <LF_SIR>.

*     Leading column -> Input label in header row
      ELSEIF LS_SIC-LEADCOL IS NOT INITIAL.
        PERFORM SITAB_SHOW_LEADING_CELL
          USING LS_SIC
          CHANGING <LF_SIR>.

      ELSE.
*       Nonleading column -> show dimension setting
        PERFORM SITAB_SHOW_COLUMN_SETTING
          USING LS_SIC
          CHANGING <LF_SIR>.

      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0210_PBO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM 0210_PBO .
  DATA:
    LS_LAYOUT    TYPE LVC_S_LAYO,
    LS_LAYO_CHAR TYPE LVC_S_LAYO,
    LT_FCAT_COND TYPE LVC_T_FCAT,
    LT_FCAT_CHAR TYPE LVC_T_FCAT,
    LS_VAR_COND  TYPE  DISVARIANT,
    LS_VAR_CHAR  TYPE  DISVARIANT.

  LS_LAYOUT-NO_TOOLBAR    = 'X'.
  LS_LAYOUT-CWIDTH_OPT    = 'X'.
  LS_LAYOUT-GRID_TITLE    = TEXT-001.
  LS_LAYO_CHAR            = LS_LAYOUT.
  LS_LAYO_CHAR-GRID_TITLE = TEXT-002.
  LS_VAR_COND-REPORT      = LS_VAR_CHAR-REPORT = SY-REPID.
  LS_VAR_COND-HANDLE      = 'COND'.
  LS_VAR_CHAR-HANDLE      = 'CHAR'.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME       = 'ZST0_BM_RPC_SELOPT'
    CHANGING
      CT_FIELDCAT            = LT_FCAT_COND
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.
  LOOP AT LT_FCAT_COND ASSIGNING FIELD-SYMBOL(<LF_FCAT>).
    CASE <LF_FCAT>-FIELDNAME.
      WHEN 'LOW' OR 'HIGH'.
        <LF_FCAT>-EDIT = 'X'.
      WHEN 'FIELDNAME' OR 'CONDID' OR 'TEXT' OR 'TO_TEXT'.
        <LF_FCAT>-NO_OUT = SPACE.
      WHEN 'OPTI_PUSH' OR 'VALU_PUSH'.
        <LF_FCAT>-STYLE       = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
      WHEN OTHERS.
        <LF_FCAT>-NO_OUT = 'X'.
    ENDCASE.
  ENDLOOP.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME       = 'ZST_BM_RPC_DIMFIELD'
    CHANGING
      CT_FIELDCAT            = LT_FCAT_CHAR
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.
  LOOP AT LT_FCAT_CHAR ASSIGNING <LF_FCAT>.
    CASE <LF_FCAT>-FIELDNAME.
      WHEN 'FIELDNAME' OR 'SCRTEXT_L'.
        <LF_FCAT>-NO_OUT = SPACE.
      WHEN OTHERS.
        <LF_FCAT>-NO_OUT = 'X'.
    ENDCASE.
  ENDLOOP.

  IF GO_SI_COND IS INITIAL.
    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR'
      EXPORTING
        I_CPROG                 = SY-REPID
        I_DYNNR                 = '0200' "SY-DYNNR
        I_CUS_CONTROL_NAME      = 'ALV_LEFT'
        IS_LAYOUT               = LS_LAYOUT
        IS_VARIANT              = LS_VAR_COND
        I_CALLBACK_PROGRAM      = SY-REPID
        I_CALLBACK_BUTTON_CLICK = '0210_ALV_COND_PUSH'
      IMPORTING
        E_ALV_GRID              = GO_SI_COND
      CHANGING
        IT_OUTTAB               = GT_CONDITION
        IT_FIELDCATALOG         = LT_FCAT_COND.

    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR'
      EXPORTING
        I_CPROG            = SY-REPID
        I_DYNNR            = '0200'
        I_CUS_CONTROL_NAME = 'ALV_RIGHT'
        IS_LAYOUT          = LS_LAYO_CHAR
        IS_VARIANT         = LS_VAR_CHAR
      IMPORTING
        E_ALV_GRID         = GO_SI_CHAR
      CHANGING
        IT_OUTTAB          = GT_RPC_FCHAR
        IT_FIELDCATALOG    = LT_FCAT_CHAR.
  ELSE.
    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
      EXPORTING
        I_ALV_GRID = GO_SI_COND.

    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
      EXPORTING
        I_ALV_GRID = GO_SI_CHAR.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0210_VALUE_COND_PUSH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ES_COL_ID  text
*      -->ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM 0210_ALV_COND_PUSH
  USING   ES_COL_ID TYPE  LVC_S_COL
          ES_ROW_NO TYPE  LVC_S_ROID.

  CASE ES_COL_ID-FIELDNAME.
    WHEN 'OPTI_PUSH'.
      PERFORM 0210_OPTI_COND_PUSH
        USING ES_COL_ID
              ES_ROW_NO.
    WHEN 'VALU_PUSH'.
      PERFORM 0210_VALUE_COND_PUSH
        USING ES_COL_ID
              ES_ROW_NO.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0220_ALV_FACTOR_PUSH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ES_COL_ID  text
*      -->ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM 0220_ALV_FACTOR_PUSH
  USING   ES_COL_ID TYPE  LVC_S_COL
          ES_ROW_NO TYPE  LVC_S_ROID.
  CHECK ES_COL_ID-FIELDNAME = 'FIELDNAME'.

  READ TABLE GT_FACTOR ASSIGNING FIELD-SYMBOL(<LF_FACTOR>)
    INDEX ES_ROW_NO-ROW_ID.
  CHECK SY-SUBRC IS INITIAL.
  PERFORM 0220_INSERT_ELEMENT USING <LF_FACTOR>.
*  CONCATENATE ZST0_BM_RPC_DIMSET-FORMULA <LF_FACTOR>
*         INTO ZST0_BM_RPC_DIMSET-FORMULA SEPARATED BY SPACE.

  LEAVE TO SCREEN 0200.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0210_OPTI_COND_PUSH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ES_COL_ID  text
*      -->ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM 0210_OPTI_COND_PUSH
  USING   ES_COL_ID TYPE  LVC_S_COL
          ES_ROW_NO TYPE  LVC_S_ROID.
  CHECK ES_COL_ID-FIELDNAME = 'OPTI_PUSH'.

  READ TABLE GT_CONDITION ASSIGNING FIELD-SYMBOL(<LF_SELOPT>)
    INDEX ES_ROW_NO-ROW_ID.
  CHECK SY-SUBRC IS INITIAL.
  READ TABLE GS_RPC_DS_SI-CONDITIONS ASSIGNING FIELD-SYMBOL(<LF_SICOND>)
    WITH KEY ROWNO  = ZST0_BM_RPC_DIMSET-ROWNO
             RFIELD = <LF_SELOPT>-FIELDNAME
             RANGID = 1.
  CHECK SY-SUBRC IS INITIAL.

  CALL FUNCTION 'ZFM_BM_POPUP_RANGE_OPTION'
    CHANGING
      C_SIGN   = <LF_SICOND>-RSIGN
      C_OPTION = <LF_SICOND>-ROPTI
      C_HIGH   = <LF_SICOND>-RHIGH.

  PERFORM GEN_CONDITION_ALV.
  CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
    EXPORTING
      I_ALV_GRID = GO_SI_COND.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0210_VALUE_COND_PUSH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ES_COL_ID  text
*      -->ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM 0210_VALUE_COND_PUSH
  USING   ES_COL_ID TYPE  LVC_S_COL
          ES_ROW_NO TYPE  LVC_S_ROID.
  DATA:
    LS_SELOPT     TYPE ZST0_BM_RPC_SELOPT,
    LT_SICOND     TYPE TABLE OF ZST0_BM_RPC_SICOND,
    LS_RSTABFIELD TYPE RSTABFIELD,
    LR_BUKRS      TYPE RANGE OF ACDOCA-RBUKRS,
    LS_RANGE      TYPE RSDSSELOPT,
    LR_RANGE      TYPE RSDSSELOPT_T.

  CHECK ES_COL_ID-FIELDNAME = 'VALU_PUSH'.

  READ TABLE GT_CONDITION INTO LS_SELOPT INDEX ES_ROW_NO-ROW_ID.
  LOOP AT GS_RPC_DS_SI-CONDITIONS ASSIGNING FIELD-SYMBOL(<LF_SRC_COND>)
    WHERE ROWNO   = ZST0_BM_RPC_DIMSET-ROWNO
      AND COLNO   = ZST0_BM_RPC_DIMSET-COLNO
      AND RFIELD  = LS_SELOPT-FIELDNAME
      AND CONDID  = LS_SELOPT-CONDID.
    <LF_SRC_COND>-RSIGN  = LS_SELOPT-SIGN.
    <LF_SRC_COND>-ROPTI  = LS_SELOPT-OPTION.
    <LF_SRC_COND>-RLOW   = LS_SELOPT-LOW.
    <LF_SRC_COND>-RHIGH  = LS_SELOPT-HIGH.
    EXIT.
  ENDLOOP.

  LT_SICOND = GS_RPC_DS_SI-CONDITIONS.
  DELETE LT_SICOND
    WHERE ROWNO <> ZST0_BM_RPC_DIMSET-ROWNO
       OR COLNO <> ZST0_BM_RPC_DIMSET-COLNO.
  DELETE LT_SICOND
    WHERE RFIELD <> LS_SELOPT-FIELDNAME
       OR CONDID  <> LS_SELOPT-CONDID.

  READ TABLE GT_RPC_CHAR INTO DATA(LS_CHAR)
    WITH KEY TABNAME = ZST0_BM_RPC_DIMSET-SRCTAB
             FIELDNAME = LS_SELOPT-FIELDNAME BINARY SEARCH.
  IF LS_CHAR-CHECKTABLE IS NOT INITIAL.
    LS_RSTABFIELD-TABLENAME = LS_CHAR-CHECKTABLE.
    LS_RSTABFIELD-FIELDNAME = LS_CHAR-CHECKFIELD.
  ELSE.
    LS_RSTABFIELD-TABLENAME = ZST0_BM_RPC_DIMSET-SRCTAB.
    LS_RSTABFIELD-FIELDNAME = LS_SELOPT-FIELDNAME.
  ENDIF.

  DELETE LT_SICOND INDEX 1.
  MOVE-CORRESPONDING LS_SELOPT TO LS_RANGE.
  IF LS_RANGE IS NOT INITIAL.
    APPEND LS_RANGE TO LR_RANGE.
  ENDIF.
  LOOP AT LT_SICOND INTO DATA(LS_SICOND).
    LS_RANGE-SIGN    = LS_SICOND-RSIGN.
    LS_RANGE-OPTION  = LS_SICOND-ROPTI.
    LS_RANGE-LOW     = LS_SICOND-RLOW.
    LS_RANGE-HIGH    = LS_SICOND-RHIGH.
    APPEND LS_RANGE TO LR_RANGE.
  ENDLOOP.

  CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
    EXPORTING
      TITLE             = 'Selection'
      TEXT              = 'Option'
      TAB_AND_FIELD     = LS_RSTABFIELD
    TABLES
      RANGE             = LR_RANGE
    EXCEPTIONS
      NO_RANGE_TAB      = 1
      CANCELLED         = 2
      INTERNAL_ERROR    = 3
      INVALID_FIELDNAME = 4
      OTHERS            = 5.

  CHECK LR_RANGE[] IS NOT INITIAL.
  DELETE GS_RPC_DS_SI-CONDITIONS
    WHERE ROWNO   = ZST0_BM_RPC_DIMSET-ROWNO
      AND COLNO   = ZST0_BM_RPC_DIMSET-COLNO
      AND RFIELD  = LS_SELOPT-FIELDNAME
      AND CONDID  = LS_SELOPT-CONDID.
  LOOP AT LR_RANGE INTO LS_RANGE.
    CLEAR: LS_SICOND.
    LS_SICOND-COLNO   = ZST0_BM_RPC_DIMSET-COLNO.
    LS_SICOND-ROWNO   = ZST0_BM_RPC_DIMSET-ROWNO.
    LS_SICOND-RTABLE  = ZST0_BM_RPC_DIMSET-SRCTAB.
    LS_SICOND-CONDID  = LS_SELOPT-CONDID.
    LS_SICOND-RFIELD  = LS_SELOPT-FIELDNAME.
    LS_SICOND-RANGID  = SY-TABIX.
    LS_SICOND-RSIGN   = LS_RANGE-SIGN.
    LS_SICOND-ROPTI   = LS_RANGE-OPTION.
    LS_SICOND-RLOW    = LS_RANGE-LOW.
    LS_SICOND-RHIGH   = LS_RANGE-HIGH.
    APPEND LS_SICOND TO GS_RPC_DS_SI-CONDITIONS.
  ENDLOOP.

  PERFORM GEN_CONDITION_ALV.
  CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
    EXPORTING
      I_ALV_GRID = GO_SI_COND.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GEN_CONDITION_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LS_SIR  text
*----------------------------------------------------------------------*
FORM GEN_CONDITION_ALV.
  DATA:
    LT_SICOND    TYPE TABLE OF ZST0_BM_RPC_SICOND,
    LS_SELOPT    TYPE ZST0_BM_RPC_SELOPT,
    LW_FIELDNAME TYPE CHAR61,
    LW_SCRTEXT_L TYPE SCRTEXT_L.

  CLEAR: GT_CONDITION.

  LT_SICOND = GS_RPC_DS_SI-CONDITIONS.
  DELETE LT_SICOND
    WHERE ROWNO <> ZST0_BM_RPC_DIMSET-ROWNO
       OR COLNO <> ZST0_BM_RPC_DIMSET-COLNO.
  SORT LT_SICOND BY CONDID RFIELD.

  GT_RPC_FCHAR = GT_RPC_CHAR.
  DELETE GT_RPC_FCHAR WHERE TABNAME <> ZST0_BM_RPC_DIMSET-SRCTAB.
  LOOP AT LT_SICOND ASSIGNING FIELD-SYMBOL(<LF_COND>).
    AT NEW RFIELD.
      CLEAR: LS_SELOPT.
      LS_SELOPT-FIELDNAME = <LF_COND>-RFIELD.
      LS_SELOPT-CONDID    = <LF_COND>-CONDID.
      READ TABLE GT_RPC_CHAR INTO DATA(LS_CHAR)
        WITH KEY TABNAME   = ZST0_BM_RPC_DIMSET-SRCTAB
                 FIELDNAME = <LF_COND>-RFIELD BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        LS_SELOPT-TEXT    = LS_CHAR-SCRTEXT_L.
      ENDIF.
      LS_SELOPT-TO_TEXT = 'To'.
      DELETE GT_RPC_FCHAR WHERE FIELDNAME = <LF_COND>-RFIELD.
    ENDAT.

    IF LS_SELOPT-SIGN IS INITIAL.
      LS_SELOPT-SIGN    = <LF_COND>-RSIGN.
      LS_SELOPT-OPTION  = <LF_COND>-ROPTI.
      LS_SELOPT-LOW     = <LF_COND>-RLOW.
      LS_SELOPT-HIGH    = <LF_COND>-RHIGH.
      CALL FUNCTION 'SELSCREEN_ICONS_SUPPLY'
        EXPORTING
          SIGN           = LS_SELOPT-SIGN
          OPTION         = LS_SELOPT-OPTION
        IMPORTING
          ICON_RESULT    = LS_SELOPT-OPTI_PUSH
        EXCEPTIONS
          ILLEGAL_SIGN   = 1
          ILLEGAL_OPTION = 2
          OTHERS         = 3.
      CALL FUNCTION 'ICON_CREATE'
        EXPORTING
          NAME                  = 'ICON_ENTER_MORE'
        IMPORTING
          RESULT                = LS_SELOPT-VALU_PUSH
        EXCEPTIONS
          ICON_NOT_FOUND        = 1
          OUTPUTFIELD_TOO_SHORT = 2
          OTHERS                = 3.
    ELSE.
      CALL FUNCTION 'ICON_CREATE'
        EXPORTING
          NAME                  = 'ICON_DISPLAY_MORE'
        IMPORTING
          RESULT                = LS_SELOPT-VALU_PUSH
        EXCEPTIONS
          ICON_NOT_FOUND        = 1
          OUTPUTFIELD_TOO_SHORT = 2
          OTHERS                = 3.
    ENDIF.

    AT END OF RFIELD.
      APPEND LS_SELOPT TO GT_CONDITION.
    ENDAT.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0210_FC_MOVE_ALL_RIGHT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0210_FC_MOVE_ALL_RIGHT.

  DELETE GS_RPC_DS_SI-CONDITIONS
    WHERE ROWNO = ZST0_BM_RPC_DIMSET-ROWNO
      AND COLNO = ZST0_BM_RPC_DIMSET-COLNO.

  PERFORM GEN_CONDITION_ALV.
  CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
    EXPORTING
      I_ALV_GRID = GO_SI_COND.

  CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
    EXPORTING
      I_ALV_GRID = GO_SI_CHAR.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0210_FC_MOVE_RIGHT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0210_FC_MOVE_RIGHT .
  DATA:
    LW_ROW    TYPE I.

  CALL METHOD GO_SI_COND->GET_CURRENT_CELL
    IMPORTING
      E_ROW = LW_ROW.

  READ TABLE GT_CONDITION INTO DATA(LS_COND) INDEX LW_ROW.
  CHECK SY-SUBRC IS INITIAL.

  DELETE GS_RPC_DS_SI-CONDITIONS
    WHERE ROWNO = ZST0_BM_RPC_DIMSET-ROWNO
      AND COLNO = ZST0_BM_RPC_DIMSET-COLNO
      AND RFIELD = LS_COND-FIELDNAME
      AND CONDID = LS_COND-CONDID.

  PERFORM GEN_CONDITION_ALV.
  CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
    EXPORTING
      I_ALV_GRID = GO_SI_COND.

  CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
    EXPORTING
      I_ALV_GRID = GO_SI_CHAR.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0210_FC_MOVE_LEFT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0210_FC_MOVE_LEFT.
  DATA:
    LW_ROW    TYPE I,
    LS_SICOND TYPE ZST0_BM_RPC_SICOND.

  CALL METHOD GO_SI_CHAR->GET_CURRENT_CELL
    IMPORTING
      E_ROW = LW_ROW.

  READ TABLE GT_RPC_FCHAR INTO DATA(LS_CHAR) INDEX LW_ROW.
  IF SY-SUBRC IS INITIAL.
    CLEAR: LS_SICOND.
    LS_SICOND-COLNO   = ZST0_BM_RPC_DIMSET-COLNO.
    LS_SICOND-ROWNO   = ZST0_BM_RPC_DIMSET-ROWNO.
    LS_SICOND-RTABLE  = ZST0_BM_RPC_DIMSET-SRCTAB.
    LS_SICOND-RFIELD  = LS_CHAR-FIELDNAME.
    LS_SICOND-RANGID  = 1.
    APPEND LS_SICOND TO GS_RPC_DS_SI-CONDITIONS.
    PERFORM GEN_CONDITION_ALV.
    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
      EXPORTING
        I_ALV_GRID = GO_SI_COND.

    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
      EXPORTING
        I_ALV_GRID = GO_SI_CHAR.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0210_FC_MOVE_LEFT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0210_FC_MOVE_ALL_LEFT.
  DATA:
    LS_SICOND TYPE ZST0_BM_RPC_SICOND.

  LOOP AT GT_RPC_FCHAR INTO DATA(LS_CHAR).
    CLEAR: LS_SICOND.
    LS_SICOND-COLNO   = ZST0_BM_RPC_DIMSET-COLNO.
    LS_SICOND-ROWNO   = ZST0_BM_RPC_DIMSET-ROWNO.
    LS_SICOND-RTABLE  = ZST0_BM_RPC_DIMSET-SRCTAB.
    LS_SICOND-RFIELD  = LS_CHAR-FIELDNAME.
    LS_SICOND-RANGID  = 1.
    APPEND LS_SICOND TO GS_RPC_DS_SI-CONDITIONS.
  ENDLOOP.

  PERFORM GEN_CONDITION_ALV.
  CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
    EXPORTING
      I_ALV_GRID = GO_SI_COND.

  CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
    EXPORTING
      I_ALV_GRID = GO_SI_CHAR.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SEGMENT_ITEM_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SEGMENT_ITEM_SAVE
  USING  LPS_SI_NEW TYPE GTY_SI_DB
         LPS_SI_ORD TYPE GTY_SI_DB.

  UPDATE ZTB_BM_RPC_SI FROM LPS_SI_NEW-SITEM.
  IF SY-SUBRC IS NOT INITIAL.
    MESSAGE S011(ZMS_COL_LIB) WITH 'ZTB_BM_RPC_SI'.
    ROLLBACK WORK.
    RETURN.
  ENDIF.

  IF LPS_SI_NEW-SIROW <> LPS_SI_ORD-SIROW.
    CALL FUNCTION 'ZFM_DATA_UPDATE_TABLE'
      EXPORTING
        I_STRUCTURE      = 'ZTB_BM_RPC_SIR'
        T_TABLE_CHANGED  = LPS_SI_NEW-SIROW
        T_TABLE_ORIGINAL = LPS_SI_ORD-SIROW
        I_ASSIGN_RQ      = 'X'
      EXCEPTIONS
        OTHERS           = 10.
    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE S011(ZMS_COL_LIB) WITH 'ZTB_BM_RPC_SIR' DISPLAY LIKE 'E'.
      ROLLBACK WORK.
      RETURN.
    ENDIF.
  ENDIF.

  IF LPS_SI_NEW-SIRT <> LPS_SI_ORD-SIRT.
    CALL FUNCTION 'ZFM_DATA_UPDATE_TABLE'
      EXPORTING
        I_STRUCTURE      = 'ZTB_BM_RPC_SIRT'
        T_TABLE_CHANGED  = LPS_SI_NEW-SIRT
        T_TABLE_ORIGINAL = LPS_SI_ORD-SIRT
        I_ASSIGN_RQ      = 'X'
      EXCEPTIONS
        OTHERS           = 10.
    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE S011(ZMS_COL_LIB) WITH 'ZTB_BM_RPC_SIRT' DISPLAY LIKE 'E'.
      ROLLBACK WORK.
      RETURN.
    ENDIF.
  ENDIF.

  IF LPS_SI_NEW-SIRCOND <> LPS_SI_ORD-SIRCOND.
    CALL FUNCTION 'ZFM_DATA_UPDATE_TABLE'
      EXPORTING
        I_STRUCTURE      = 'ZTB_BM_RPC_SIRC'
        T_TABLE_CHANGED  = LPS_SI_NEW-SIRCOND
        T_TABLE_ORIGINAL = LPS_SI_ORD-SIRCOND
        I_ASSIGN_RQ      = 'X'
      EXCEPTIONS
        OTHERS           = 10.
    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE S011(ZMS_COL_LIB) WITH 'ZTB_BM_RPC_SIRC' DISPLAY LIKE 'E'.
      ROLLBACK WORK.
      RETURN.
    ENDIF.
  ENDIF.

  IF LPS_SI_NEW-SICOL <> LPS_SI_ORD-SICOL.
    CALL FUNCTION 'ZFM_DATA_UPDATE_TABLE'
      EXPORTING
        I_STRUCTURE      = 'ZTB_BM_RPC_SIC'
        T_TABLE_CHANGED  = LPS_SI_NEW-SICOL
        T_TABLE_ORIGINAL = LPS_SI_ORD-SICOL
        I_ASSIGN_RQ      = 'X'
      EXCEPTIONS
        OTHERS           = 10.
    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE S011(ZMS_COL_LIB) WITH 'ZTB_BM_RPC_SIC' DISPLAY LIKE 'E'.
      ROLLBACK WORK.
      RETURN.
    ENDIF.
  ENDIF.

  IF LPS_SI_NEW-SICCOND <> LPS_SI_ORD-SICCOND.
    CALL FUNCTION 'ZFM_DATA_UPDATE_TABLE'
      EXPORTING
        I_STRUCTURE      = 'ZTB_BM_RPC_SICC'
        T_TABLE_CHANGED  = LPS_SI_NEW-SICCOND
        T_TABLE_ORIGINAL = LPS_SI_ORD-SICCOND
        I_ASSIGN_RQ      = 'X'
      EXCEPTIONS
        OTHERS           = 10.
    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE S011(ZMS_COL_LIB) WITH 'ZTB_BM_RPC_SICC' DISPLAY LIKE 'E'.
      ROLLBACK WORK.
      RETURN.
    ENDIF.
  ENDIF.

  GS_RPC_DS_SI_O = GS_RPC_DS_SI.

* Get SI data
  READ TABLE GS_RPC_R-SEGMENTS ASSIGNING FIELD-SYMBOL(<LF_RPC_S>)
    WITH KEY RPSEG = ZVI_BM_RPC_SI-RPSEG BINARY SEARCH.
  IF SY-SUBRC IS INITIAL.
    READ TABLE <LF_RPC_S>-ITEMS ASSIGNING FIELD-SYMBOL(<LF_RPC_SI>)
      WITH KEY RITEM = ZVI_BM_RPC_SI-RITEM BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      <LF_RPC_SI> = GS_RPC_DS_SI.
      MOVE-CORRESPONDING GS_RPC_DS_SI TO ZVI_BM_RPC_SI.
    ENDIF.
  ENDIF.
  MESSAGE S009(ZMS_COL_LIB).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CONVERT_SI_TO_DB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LPS_RPC_DS_SI  text
*      <--LPS_SITEM  text
*----------------------------------------------------------------------*
FORM CONVERT_SI_TO_DB
  USING    LPS_RPC_DS_SI  TYPE ZST0_BM_RPC_SI
  CHANGING LPS_SIDB       TYPE GTY_SI_DB.
  DATA:
    LS_SIC  TYPE ZTB_BM_RPC_SIC,
    LS_SICC TYPE ZTB_BM_RPC_SICC,
    LS_SIR  TYPE ZTB_BM_RPC_SIR,
    LS_SIRT TYPE ZTB_BM_RPC_SIRT,
    LS_SIRC TYPE ZTB_BM_RPC_SIRC.

  CLEAR: LPS_SIDB.

  MOVE-CORRESPONDING LPS_RPC_DS_SI TO LPS_SIDB-SITEM.
  LPS_SIDB-SITEM-REPID = ZVI_BM_RPC_SI-REPID.
  LPS_SIDB-SITEM-RPSEG = ZVI_BM_RPC_SI-RPSEG.

  LOOP AT LPS_RPC_DS_SI-ROWS INTO DATA(LS_SIROW).
    CLEAR: LS_SIR.
    MOVE-CORRESPONDING LS_SIROW TO LS_SIR.
    LS_SIR-REPID = ZVI_BM_RPC_SI-REPID.
    LS_SIR-RPSEG = ZVI_BM_RPC_SI-RPSEG.
    LS_SIR-RITEM = ZVI_BM_RPC_SI-RITEM.
    APPEND LS_SIR TO LPS_SIDB-SIROW.

    CLEAR: LS_SIRT.
    MOVE-CORRESPONDING LS_SIROW TO LS_SIRT.
    MOVE-CORRESPONDING LS_SIR TO LS_SIRT.
    LS_SIRT-LANGU = 'E'.
    APPEND LS_SIRT TO LPS_SIDB-SIRT.

    IF LS_SIROW-VIDATA IS NOT INITIAL.
      CLEAR: LS_SIRT.
      MOVE-CORRESPONDING LS_SIROW TO LS_SIRT.
      MOVE-CORRESPONDING LS_SIR TO LS_SIRT.
      MOVE-CORRESPONDING LS_SIROW-VIDATA TO LS_SIRT.
      LS_SIRT-LANGU = GC_LANGU_VN.
      APPEND LS_SIRT TO LPS_SIDB-SIRT.
    ENDIF.
  ENDLOOP.

  LOOP AT LPS_RPC_DS_SI-CONDITIONS INTO DATA(LS_SICOND)
    WHERE COLNO IS INITIAL.
    CLEAR: LS_SIRC.

    PERFORM CONDITION_CONVERSION_EXIT
      CHANGING LS_SICOND.

    MOVE-CORRESPONDING LS_SICOND TO LS_SIRC.
    LS_SIRC-REPID = ZVI_BM_RPC_SI-REPID.
    LS_SIRC-RPSEG = ZVI_BM_RPC_SI-RPSEG.
    LS_SIRC-RITEM = ZVI_BM_RPC_SI-RITEM.
    APPEND LS_SIRC TO LPS_SIDB-SIRCOND.
  ENDLOOP.

  LOOP AT LPS_RPC_DS_SI-COLUMNS INTO DATA(LS_SICOL).
    CLEAR: LS_SIC.

    MOVE-CORRESPONDING LS_SICOL TO LS_SIC.
    LS_SIC-REPID = ZVI_BM_RPC_SI-REPID.
    LS_SIC-RPSEG = ZVI_BM_RPC_SI-RPSEG.
    LS_SIC-RITEM = ZVI_BM_RPC_SI-RITEM.
    APPEND LS_SIC TO LPS_SIDB-SICOL.
  ENDLOOP.

  LOOP AT LPS_RPC_DS_SI-CONDITIONS INTO LS_SICOND
    WHERE ROWNO IS INITIAL.
    CLEAR: LS_SICC.

    PERFORM CONDITION_CONVERSION_EXIT
      CHANGING LS_SICOND.

    MOVE-CORRESPONDING LS_SICOND TO LS_SICC.
    LS_SICC-REPID = ZVI_BM_RPC_SI-REPID.
    LS_SICC-RPSEG = ZVI_BM_RPC_SI-RPSEG.
    LS_SICC-RITEM = ZVI_BM_RPC_SI-RITEM.
    APPEND LS_SICC TO LPS_SIDB-SICCOND.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CONVERT_SI_TO_DB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LPS_RPC_FORM  text
*      <--LPS_SITEM  text
*----------------------------------------------------------------------*
FORM CONVERT_FS_TO_DB
  USING    LPS_RPC_FORM   TYPE ZST0_BM_RPC_F
           LPW_NEW        TYPE XMARK
  CHANGING LPS_FSDB       TYPE GTY_FS_DB.
  DATA:
    LS_DATA   TYPE ZTB_BM_RPC_D,
    LS_DATA_D TYPE ZST0_BM_RPC_D.

  CLEAR: LPS_FSDB.

  MOVE-CORRESPONDING LPS_RPC_FORM TO LPS_FSDB-FORMS.
  LPS_FSDB-FORMS-REPID = ZVI_BM_RPC_F-REPID.
  LPS_FSDB-FORMS-RPSEG = ZVI_BM_RPC_F-RPSEG.

  LOOP AT LPS_RPC_FORM-CELLS INTO DATA(LS_CELL)
    WHERE HROW IS INITIAL.
    READ TABLE LPS_RPC_FORM-DATA ASSIGNING FIELD-SYMBOL(<LF_DATA>)
      WITH KEY ROWPOS = LS_CELL-ROWNO.
    IF SY-SUBRC IS INITIAL.
      CLEAR: LS_DATA.
      MOVE-CORRESPONDING LS_CELL TO <LF_DATA>.
      MOVE-CORRESPONDING <LF_DATA> TO LS_DATA.
      LS_DATA-REPID = ZVI_BM_RPC_F-REPID.
      LS_DATA-RPSEG = ZVI_BM_RPC_F-RPSEG.
      LS_DATA-RITEM = ZVI_BM_RPC_F-RITEM.
      LS_DATA-EFMON = ZVI_BM_RPC_F-EFMON.
      LS_DATA-LANGU = SPACE.
      LS_DATA-ROWPOS = LS_CELL-ROWNO.
      APPEND LS_DATA TO LPS_FSDB-DATA.
    ELSEIF LPW_NEW IS NOT INITIAL.
      CLEAR: LS_DATA.
      MOVE-CORRESPONDING LS_CELL TO LS_DATA.
      LS_DATA-REPID = ZVI_BM_RPC_F-REPID.
      LS_DATA-RPSEG = ZVI_BM_RPC_F-RPSEG.
      LS_DATA-RITEM = ZVI_BM_RPC_F-RITEM.
      LS_DATA-EFMON = ZVI_BM_RPC_F-EFMON.
      LS_DATA-LANGU = SPACE.
      LS_DATA-ROWPOS = LS_CELL-ROWNO.
      APPEND LS_DATA TO LPS_FSDB-DATA.
      MOVE-CORRESPONDING LS_DATA TO LS_DATA_D.
      APPEND LS_DATA_D TO LPS_RPC_FORM-DATA.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CONVERT_FS_CELLS_TO_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LPS_RPC_FORM  text
*      <--LPS_SITEM  text
*----------------------------------------------------------------------*
FORM CONVERT_FS_CELLS_TO_DATA
  USING    LPS_RPC_FORM   TYPE ZST0_BM_RPC_F.

  LOOP AT LPS_RPC_FORM-CELLS INTO DATA(LS_CELL)
    WHERE HROW IS INITIAL.
    READ TABLE LPS_RPC_FORM-DATA ASSIGNING FIELD-SYMBOL(<LF_DATA>)
      WITH KEY ROWPOS = LS_CELL-ROWNO.
    IF SY-SUBRC IS INITIAL.
      MOVE-CORRESPONDING LS_CELL TO <LF_DATA>.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0200_CANCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM 0200_CANCEL .
  DATA:
    LT_RCOND TYPE TABLE OF ZST0_BM_RPC_SICOND,
    LT_CCOND TYPE TABLE OF ZST0_BM_RPC_SICOND.

  IF ZST0_BM_RPC_DIMSET-ROWNO IS INITIAL.
    READ TABLE GS_RPC_DS_SI-COLUMNS ASSIGNING FIELD-SYMBOL(<LF_SICOL>)
      WITH KEY COLNO = ZST0_BM_RPC_DIMSET-COLNO.
    IF SY-SUBRC IS INITIAL.
      READ TABLE GS_RPC_DS_SI_O-COLUMNS INTO DATA(LS_SICOL)
        WITH KEY COLNO = ZST0_BM_RPC_DIMSET-COLNO.
      IF SY-SUBRC IS INITIAL.
        <LF_SICOL> = LS_SICOL.
      ENDIF.
    ENDIF.

    DELETE GS_RPC_DS_SI-CONDITIONS WHERE COLNO = ZST0_BM_RPC_DIMSET-COLNO.
    LT_CCOND = GS_RPC_DS_SI_O-CONDITIONS.
    DELETE LT_CCOND WHERE COLNO <> ZST0_BM_RPC_DIMSET-COLNO.
    APPEND LINES OF LT_CCOND TO GS_RPC_DS_SI-CONDITIONS.
  ELSE.
    READ TABLE GS_RPC_DS_SI-ROWS ASSIGNING FIELD-SYMBOL(<LF_SIROW>)
      WITH KEY ROWNO = ZST0_BM_RPC_DIMSET-ROWNO
               HROW  = SPACE.
    IF SY-SUBRC IS INITIAL.
      READ TABLE GS_RPC_DS_SI_O-ROWS INTO DATA(LS_SIROW)
        WITH KEY ROWNO = ZST0_BM_RPC_DIMSET-ROWNO
                 HROW  = SPACE.
      IF SY-SUBRC IS INITIAL.
        <LF_SIROW> = LS_SIROW.
      ENDIF.
    ENDIF.

    DELETE GS_RPC_DS_SI-CONDITIONS WHERE ROWNO = ZST0_BM_RPC_DIMSET-ROWNO.
    LT_RCOND = GS_RPC_DS_SI_O-CONDITIONS.
    DELETE LT_RCOND WHERE ROWNO <> ZST0_BM_RPC_DIMSET-ROWNO.
    APPEND LINES OF LT_RCOND TO GS_RPC_DS_SI-CONDITIONS.
  ENDIF.

  LEAVE TO SCREEN 0.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0200_OK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0200_OK .
  DATA:
    LW_INDEX    TYPE I.

  IF ZST0_BM_RPC_DIMSET-FIGTY = GC_FIGTY_FORMULA.
    PERFORM 0220_PARSE_FORMULA.
  ELSEIF ZST0_BM_RPC_DIMSET-FIGTY = GC_FIGTY_KFNCP
  OR ZST0_BM_RPC_DIMSET-FIGTY = GC_FIGTY_KFWCP .
    LOOP AT GT_CONDITION INTO DATA(LS_SELOPT).
      LOOP AT GS_RPC_DS_SI-CONDITIONS ASSIGNING FIELD-SYMBOL(<LF_SRC_COND>)
        WHERE ROWNO   = ZST0_BM_RPC_DIMSET-ROWNO
          AND COLNO   = ZST0_BM_RPC_DIMSET-COLNO
          AND RFIELD  = LS_SELOPT-FIELDNAME
          AND CONDID  = LS_SELOPT-CONDID.
        <LF_SRC_COND>-RSIGN  = LS_SELOPT-SIGN.
        <LF_SRC_COND>-ROPTI  = LS_SELOPT-OPTION.
        <LF_SRC_COND>-RLOW   = LS_SELOPT-LOW.
        <LF_SRC_COND>-RHIGH  = LS_SELOPT-HIGH.

        PERFORM CONDITION_CONVERSION_EXIT
          CHANGING <LF_SRC_COND>.
        EXIT.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

  IF ZST0_BM_RPC_DIMSET-ROWNO IS INITIAL.
    READ TABLE GS_RPC_DS_SI-COLUMNS ASSIGNING FIELD-SYMBOL(<LF_SICOL>)
      WITH KEY COLNO = ZST0_BM_RPC_DIMSET-COLNO.
    IF SY-SUBRC IS INITIAL.
      MOVE-CORRESPONDING ZST0_BM_RPC_DIMSET TO <LF_SICOL>.
    ENDIF.
  ELSE.
    READ TABLE GS_RPC_DS_SI-ROWS ASSIGNING FIELD-SYMBOL(<LF_SIROW>)
      WITH KEY ROWNO = ZST0_BM_RPC_DIMSET-ROWNO
               HROW  = SPACE.
    IF SY-SUBRC IS INITIAL.
      MOVE-CORRESPONDING ZST0_BM_RPC_DIMSET TO <LF_SIROW>.

      DO GC_MAXCOL TIMES.
        LW_INDEX = SY-INDEX - 1.
        <LF_SIROW>-LSTYLE+LW_INDEX(1) = ZST0_BM_RPC_DIMSET-CSTYLE.
      ENDDO.
      <LF_SIROW>-LSTYLE = GS_LSTYLE.

      PERFORM SI_GEN_DISPLAY_CELLS.
      CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
        EXPORTING
          I_ALV_GRID = GO_SI_ALV.
    ENDIF.
  ENDIF.

  LEAVE TO SCREEN 0.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SITAB_SHOW_ROW_SETTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  LS_SIR        text
*----------------------------------------------------------------------*
FORM SITAB_SHOW_ROW_SETTING
  USING LS_SIR  TYPE ZST0_BM_RPC_SIR.

  CLEAR: ZST0_BM_RPC_DIMSET.
  MOVE-CORRESPONDING LS_SIR TO ZST0_BM_RPC_DIMSET.
  ZST0_BM_RPC_DIMSET-CSTYLE = LS_SIR-LSTYLE.
  GS_LSTYLE = LS_SIR-LSTYLE.
  PERFORM GEN_CONDITION_ALV.

  PERFORM GEN_FACTOR_ALV.
  CALL SCREEN 0200 STARTING AT 5 2.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SITAB_SHOW_HROW_SETTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LS_SIC  text
*      -->LS_SIR  text
*----------------------------------------------------------------------*
FORM SITAB_SHOW_HROW_SETTING
  CHANGING LS_SIR  TYPE ZST0_BM_RPC_SIR.
  DATA:
    LW_ANSWER TYPE C,
    LW_INDEX  TYPE I.

  CLEAR: ZST0_BM_RPC_DIMSET.

  ZST0_BM_RPC_DIMSET-CSTYLE = LS_SIR-LSTYLE+LW_INDEX(1).

  CALL FUNCTION 'ZFM_POPUP_SET_DATA_RECORD'
    EXPORTING
      I_POPUP_TITLE  = TEXT-003
      I_SUB_TABNAME  = 'ZST0_BM_RPC_DIMSET' "'ZST0_BM_RPC_SIR'
      I_SUB_FNAME    = 'CSTYLE' "LS_SIC-FIELDNAME
      I_SUB_REQUIRED = 'X'
    IMPORTING
      RETURNCODE     = LW_ANSWER
    CHANGING
      C_RECORD       = ZST0_BM_RPC_DIMSET.
  IF LW_ANSWER IS INITIAL.
    LS_SIR-LSTYLE    = ZST0_BM_RPC_DIMSET-CSTYLE.
    DO 30 TIMES.
      LW_INDEX = SY-INDEX - 1.
      LS_SIR-LSTYLE+LW_INDEX(1) = ZST0_BM_RPC_DIMSET-CSTYLE.
    ENDDO.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SITAB_SHOW_LEADING_CELL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LS_SIC  text
*      -->LS_SIR  text
*----------------------------------------------------------------------*
FORM SITAB_SHOW_LEADING_CELL
  USING LS_SIC  TYPE ZST0_BM_RPC_SIC
  CHANGING LS_SIR  TYPE ZST0_BM_RPC_SIR.
  DATA:
    LW_ANSWER TYPE C.

  CLEAR: ZST0_BM_RPC_DIMSET.
  ASSIGN COMPONENT LS_SIC-FIELDNAME OF STRUCTURE LS_SIR
    TO FIELD-SYMBOL(<LF_VALUE>).
  IF SY-SUBRC IS INITIAL.
    ZST0_BM_RPC_DIMSET-VALEN = <LF_VALUE>.
  ENDIF.

  ASSIGN COMPONENT LS_SIC-FIELDNAME OF STRUCTURE LS_SIR-VIDATA
    TO FIELD-SYMBOL(<LF_VALUEVI>).
  IF SY-SUBRC IS INITIAL.
    ZST0_BM_RPC_DIMSET-VALVI = <LF_VALUEVI>.
  ENDIF.

  CALL FUNCTION 'ZFM_POPUP_SET_DATA_RECORD'
    EXPORTING
      I_POPUP_TITLE  = TEXT-003
      I_SUB_TABNAME  = 'ZST0_BM_RPC_DIMSET' "'ZST0_BM_RPC_SIR'
      I_SUB_FNAME    = 'VAL*' "LS_SIC-FIELDNAME
      I_SUB_REQUIRED = ' '
    IMPORTING
      RETURNCODE     = LW_ANSWER
    CHANGING
      C_RECORD       = ZST0_BM_RPC_DIMSET.
  IF LW_ANSWER IS INITIAL.
    <LF_VALUE>    = ZST0_BM_RPC_DIMSET-VALEN.
    <LF_VALUEVI>  = ZST0_BM_RPC_DIMSET-VALVI.
    PERFORM SI_GEN_DISPLAY_CELLS.
    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
      EXPORTING
        I_ALV_GRID = GO_SI_ALV.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SITAB_SHOW_COLUMN_SETTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LS_SIC  text
*      -->LS_SIR  text
*----------------------------------------------------------------------*
FORM SITAB_SHOW_COLUMN_SETTING
  USING LS_SIC  TYPE ZST0_BM_RPC_SIC
  CHANGING LS_SIR  TYPE ZST0_BM_RPC_SIR.

  CLEAR: ZST0_BM_RPC_DIMSET.
  MOVE-CORRESPONDING LS_SIC TO ZST0_BM_RPC_DIMSET.
  ASSIGN COMPONENT LS_SIC-FIELDNAME OF STRUCTURE LS_SIR
    TO FIELD-SYMBOL(<LF_VALUE>).
  IF SY-SUBRC IS INITIAL.
    ZST0_BM_RPC_DIMSET-VALEN = <LF_VALUE>.
  ENDIF.

  ASSIGN COMPONENT LS_SIC-FIELDNAME OF STRUCTURE LS_SIR-VIDATA
    TO FIELD-SYMBOL(<LF_VALUEVI>).
  IF SY-SUBRC IS INITIAL.
    ZST0_BM_RPC_DIMSET-VALVI = <LF_VALUEVI>.
  ENDIF.

  PERFORM GEN_CONDITION_ALV.
  PERFORM GEN_FACTOR_ALV.

  CALL SCREEN 0200 STARTING AT 5 2.
  IF SY-UCOMM = 'OK'.
    IF <LF_VALUE> <> ZST0_BM_RPC_DIMSET-VALEN.
      <LF_VALUE> = ZST0_BM_RPC_DIMSET-VALEN.
      PERFORM SI_GEN_DISPLAY_CELLS.
      CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
        EXPORTING
          I_ALV_GRID = GO_SI_ALV.
    ENDIF.
    <LF_VALUEVI>  = ZST0_BM_RPC_DIMSET-VALVI.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0100_CHANGE_TEMPLATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0100_CHANGE_TEMPLATE .

  READ TABLE GT_RPC_T INTO DATA(LS_RPC_T)
    WITH KEY TEMPL = ZVI_BM_RPC_SI-TEMPL BINARY SEARCH.
  IF SY-SUBRC IS INITIAL.
    GS_RPC_DS_SI-TEMPL  = ZVI_BM_RPC_SI-TEMPL.
    GS_RPC_DS_SI-NOCOL  = LS_RPC_T-NOCOL.
    GS_RPC_DS_SI-NOLEAD = LS_RPC_T-NOLEAD.
    GS_RPC_DS_SI-HROWS  = LS_RPC_T-HROWS.
  ENDIF.

  CLEAR: GS_RPC_DS_SI-COLUMNS, GS_RPC_DS_SI-ROWS,
  GS_RPC_DS_SI-CONDITIONS.

  PERFORM INIT_SI_COLS_ROWS.

  PERFORM PREPARE_OUTPUT_SI_TABLE.

  CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
    EXPORTING
      I_ALV_GRID  = GO_SI_ALV
      IT_FIELDCAT = GS_RPC_DS_SI-FCAT.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0200_CHANGE_FIGURE_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0200_CHANGE_FIGURE_TYPE .
  CASE ZST0_BM_RPC_DIMSET-FIGTY.
    WHEN GC_FIGTY_KFNCP OR GC_FIGTY_KFWCP.
      GS_FIGTY_SCR = '0210'.
    WHEN GC_FIGTY_FORMULA.
      GS_FIGTY_SCR = '0220'.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0220_PBO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0220_PBO .
  DATA:
    LS_LAYOUT    TYPE LVC_S_LAYO,
    LS_VAR_FACT  TYPE  DISVARIANT,
    LT_FCAT_FACT TYPE LVC_T_FCAT.

  LS_LAYOUT-NO_TOOLBAR    = 'X'.
*  LS_LAYOUT-CWIDTH_OPT    = 'X'.
  LS_LAYOUT-GRID_TITLE    = TEXT-004.
  LS_VAR_FACT-REPORT      = SY-REPID.
  LS_VAR_FACT-HANDLE      = 'FACT'.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME       = 'FIELDNAMES'
    CHANGING
      CT_FIELDCAT            = LT_FCAT_FACT
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.
  LOOP AT LT_FCAT_FACT ASSIGNING FIELD-SYMBOL(<LF_FCAT>).
    CASE <LF_FCAT>-FIELDNAME.
      WHEN 'FIELDNAME'.
        <LF_FCAT>-STYLE       = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
    ENDCASE.
  ENDLOOP.

  IF GO_SI_FACTOR IS INITIAL.
    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR'
      EXPORTING
        I_CPROG                 = SY-REPID
        I_DYNNR                 = '0200' "SY-DYNNR
        I_STRUCTURE_NAME        = 'FIELDNAMES'
        I_CUS_CONTROL_NAME      = 'CUS_CHAR'
        IS_LAYOUT               = LS_LAYOUT
        IS_VARIANT              = LS_VAR_FACT
        I_CALLBACK_PROGRAM      = SY-REPID
        I_CALLBACK_BUTTON_CLICK = '0220_ALV_FACTOR_PUSH'
      IMPORTING
        E_ALV_GRID              = GO_SI_FACTOR
      CHANGING
        IT_OUTTAB               = GT_FACTOR
        IT_FIELDCATALOG         = LT_FCAT_FACT.
  ELSE.
    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
      EXPORTING
        I_ALV_GRID = GO_SI_FACTOR.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GEN_FACTOR_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GEN_FACTOR_ALV .
  DATA:
    LW_NO_FACTOR TYPE I,
    LW_INDEX     TYPE NUMC3,
    LW_FIELDNAME TYPE FIELDNAME.

  CLEAR: GT_FACTOR.
  IF ZST0_BM_RPC_DIMSET-ROWNO IS INITIAL.
    LW_NO_FACTOR = GS_RPC_DS_SI-NOCOL - GS_RPC_DS_SI-NOLEAD.
    LW_NO_FACTOR = LINES( GS_RPC_DS_SI-COLUMNS ) - 1 - GS_RPC_DS_SI-NOLEAD.
    LW_INDEX = GS_RPC_DS_SI-NOLEAD.
    DO LW_NO_FACTOR TIMES.
      LW_INDEX = LW_INDEX + 1.
      CHECK LW_INDEX <> ZST0_BM_RPC_DIMSET-COLNO.
      LW_FIELDNAME = 'Y' && LW_INDEX.
      APPEND LW_FIELDNAME TO GT_FACTOR.
    ENDDO.
  ELSE.
    LOOP AT GS_RPC_DS_SI-ROWS INTO DATA(LS_SIR)
      WHERE HROW IS INITIAL
        AND ROWNO <> ZST0_BM_RPC_DIMSET-ROWNO.
      LW_FIELDNAME = 'X' && LS_SIR-ROWNO.
      APPEND LW_FIELDNAME TO GT_FACTOR.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  0220_INSERT_ELEMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LW_ELEMENT   text
*----------------------------------------------------------------------*
FORM 0220_INSERT_ELEMENT
  USING LW_ELEMENT.

  DATA: LW_FIELDNAME TYPE CHAR61,
        LW_OFFST     TYPE I,
        FLINE        LIKE CFFLT-FORML,
        LEN          TYPE I.

  GET CURSOR FIELD LW_FIELDNAME OFFSET LW_OFFST.
  IF LW_FIELDNAME EQ 'ZST0_BM_RPC_DIMSET-FORMULA'.
    IF LW_OFFST = 0.
      CONCATENATE LW_ELEMENT
                  ZST0_BM_RPC_DIMSET-FORMULA
             INTO ZST0_BM_RPC_DIMSET-FORMULA." SEPARATED BY SPACE.
    ELSE.
      CONCATENATE ZST0_BM_RPC_DIMSET-FORMULA(LW_OFFST)
                  LW_ELEMENT
                  ZST0_BM_RPC_DIMSET-FORMULA+LW_OFFST
             INTO ZST0_BM_RPC_DIMSET-FORMULA." SEPARATED BY SPACE.
    ENDIF.

  ELSE.
    CONCATENATE ZST0_BM_RPC_DIMSET-FORMULA LW_ELEMENT
           INTO ZST0_BM_RPC_DIMSET-FORMULA." SEPARATED BY SPACE.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0220_PARSE_FORMULA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0220_PARSE_FORMULA .
  DATA:
    LW_SUBRC      TYPE SY-SUBRC,
    LW_ERR_OFFSET TYPE NUMC3.

  CLEAR: GW_ERR_FACTOR.

  CALL FUNCTION 'CHECK_FORMULA'
    EXPORTING
      FORMULA          = ZST0_BM_RPC_DIMSET-FORMULA
      PROGRAM          = SY-REPID
      ROUTINE          = 'CHECK_FACTOR'
    IMPORTING
      POS              = LW_ERR_OFFSET
      SUBRC            = LW_SUBRC
    EXCEPTIONS
      ERROR_IN_FORMULA = 1
      OTHERS           = 2.
  IF SY-SUBRC <> 0.
    IF GW_ERR_FACTOR IS NOT INITIAL.
      MESSAGE S430(KX) WITH GW_ERR_FACTOR DISPLAY LIKE 'E'.
    ELSE.
      MESSAGE S430(KX) WITH LW_ERR_OFFSET DISPLAY LIKE 'E'.
    ENDIF.
    LEAVE TO SCREEN SY-DYNNR.
  ENDIF.

ENDFORM.

FORM CHECK_FACTOR
  USING TF_VARIABLE
  CHANGING LW_SUBRC  TYPE SY-SUBRC.

  READ TABLE GT_FACTOR TRANSPORTING NO FIELDS
    WITH KEY FIELDNAME = TF_VARIABLE.
  IF SY-SUBRC <> 0.
    GW_ERR_FACTOR = TF_VARIABLE.
  ENDIF.
  LW_SUBRC = SY-SUBRC.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0300_PBO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0300_PBO .
  DATA:
    LS_LAYOUT         TYPE LVC_S_LAYO.

  LS_LAYOUT-STYLEFNAME  = 'STYLEFNAME'.
  LS_LAYOUT-INFO_FNAME  = 'INFO_FNAME'.
  LS_LAYOUT-CTAB_FNAME  = 'CTAB_FNAME'.
  LS_LAYOUT-SEL_MODE    = 'D'.
  LS_LAYOUT-EDIT        = 'X'.
  LS_LAYOUT-NO_TOOLBAR  = 'X'.

  IF GO_F_ALV IS INITIAL.
    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR'
      EXPORTING
        I_CPROG                 = SY-REPID
        I_DYNNR                 = SY-DYNNR
        I_CUS_CONTROL_NAME      = 'CUS_ALV_F'
        IS_LAYOUT               = LS_LAYOUT
        I_CALLBACK_PROGRAM      = SY-REPID
        I_CALLBACK_BUTTON_CLICK = 'HANDLE_FORM_ALV_HOSPOT_CLICK'
      IMPORTING
        E_ALV_GRID              = GO_F_ALV
        E_CUS_CONTAINER         = GO_F_CON
      CHANGING
        IT_OUTTAB               = GS_RPC_IN_F-CELLS
        IT_FIELDCATALOG         = GS_RPC_IN_F-FCAT.
  ELSE.
    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
      EXPORTING
        I_ALV_GRID = GO_F_ALV.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0310_PBO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0310_PBO .
  DATA:
    LS_LAYOUT         TYPE LVC_S_LAYO.

  LS_LAYOUT-STYLEFNAME  = 'STYLEFNAME'.
  LS_LAYOUT-SEL_MODE    = 'D'.
  LS_LAYOUT-NO_TOOLBAR  = 'X'.
  LS_LAYOUT-CWIDTH_OPT  = 'X'.

  IF GO_F_ALV IS INITIAL.
    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR'
      EXPORTING
        I_CPROG                 = SY-REPID
        I_DYNNR                 = SY-DYNNR
        I_CUS_CONTROL_NAME      = 'CUS_ALV_F'
        IS_LAYOUT               = LS_LAYOUT
        I_CALLBACK_PROGRAM      = SY-REPID
        I_CALLBACK_HOSPOT_CLICK = 'HANDLE_FORM_ALV_HOSPOT_CLICK'
      IMPORTING
        E_ALV_GRID              = GO_F_ALV
        E_CUS_CONTAINER         = GO_F_CON
      CHANGING
        IT_OUTTAB               = GS_RPC_IN_F-CELLS
        IT_FIELDCATALOG         = GS_RPC_IN_F-FCAT.
  ELSE.
    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
      EXPORTING
        I_ALV_GRID = GO_F_ALV.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0320_PBO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0320_PBO .
  DATA:
    LS_LAYOUT         TYPE LVC_S_LAYO.

  LS_LAYOUT-STYLEFNAME  = 'STYLEFNAME'.
  LS_LAYOUT-SEL_MODE    = 'D'.
  LS_LAYOUT-NO_TOOLBAR  = 'X'.
  LS_LAYOUT-CWIDTH_OPT  = 'X'.

  IF GO_F_ALV IS INITIAL.
    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR'
      EXPORTING
        I_CPROG                 = SY-REPID
        I_DYNNR                 = SY-DYNNR
        I_CUS_CONTROL_NAME      = 'CUS_ALV_F'
        IS_LAYOUT               = LS_LAYOUT
        I_CALLBACK_PROGRAM      = SY-REPID
        I_CALLBACK_HOSPOT_CLICK = 'HANDLE_FORM_320_HOSPOT_CLICK'
      IMPORTING
        E_ALV_GRID              = GO_F_ALV
        E_CUS_CONTAINER         = GO_F_CON
      CHANGING
        IT_OUTTAB               = GS_RPC_IN_F-CELLS
        IT_FIELDCATALOG         = GS_RPC_IN_F-FCAT.

    CREATE OBJECT GO_TEXT_CON
      EXPORTING
        CONTAINER_NAME = 'CUS_TEXT'.

    CREATE OBJECT GO_F_TEXT
      EXPORTING
        PARENT = GO_TEXT_CON.
  ELSE.
    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
      EXPORTING
        I_ALV_GRID = GO_F_ALV.
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  0300_SAVE_FORM_STRUCTURE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0300_SAVE_FORM_STRUCTURE .
  DATA:
    LS_FS_NEW TYPE GTY_FS_DB,
    LS_FS_ORD TYPE GTY_FS_DB.
*  GS_RPC_DS_SI     TYPE ZST0_BM_RPC_SI,

  PERFORM CONVERT_FS_TO_DB
    USING GS_RPC_IN_F
          'X'
    CHANGING LS_FS_NEW.

  PERFORM CONVERT_FS_TO_DB
    USING GS_RPC_IN_F_O
          SPACE
    CHANGING LS_FS_ORD.

  UPDATE ZTB_BM_RPC_F FROM LS_FS_NEW-FORMS.
  IF SY-SUBRC IS NOT INITIAL.
    MESSAGE S011(ZMS_COL_LIB) WITH 'ZTB_BM_RPC_F' DISPLAY LIKE 'E'.
    ROLLBACK WORK.
    RETURN.
  ENDIF.

  IF LS_FS_NEW-DATA <> LS_FS_ORD-DATA.
    CALL FUNCTION 'ZFM_DATA_UPDATE_TABLE'
      EXPORTING
        I_STRUCTURE      = 'ZTB_BM_RPC_D'
        T_TABLE_CHANGED  = LS_FS_NEW-DATA
        T_TABLE_ORIGINAL = LS_FS_ORD-DATA
      EXCEPTIONS
        OTHERS           = 10.
    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE S011(ZMS_COL_LIB) WITH 'ZTB_BM_RPC_D' DISPLAY LIKE 'E'.
      ROLLBACK WORK.
      RETURN.
    ENDIF.
  ENDIF.

  LOOP AT GS_RPC_IN_F-TEXTS INTO DATA(LS_TDTEXT).
    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        HEADER   = LS_TDTEXT-THEAD
      TABLES
        LINES    = LS_TDTEXT-T_LINES
      EXCEPTIONS
        ID       = 1
        LANGUAGE = 2
        NAME     = 3
        OBJECT   = 4
        OTHERS   = 5.
  ENDLOOP.

  GS_RPC_IN_F_O = GS_RPC_IN_F.

* Get SI data
  READ TABLE GS_RPC_R-FORMSTR ASSIGNING FIELD-SYMBOL(<LF_RPC_F>)
    WITH KEY RPSEG = ZVI_BM_RPC_SI-RPSEG BINARY SEARCH.
  IF SY-SUBRC IS INITIAL.
    <LF_RPC_F> = GS_RPC_IN_F.
    MOVE-CORRESPONDING GS_RPC_IN_F TO ZVI_BM_RPC_F.
  ENDIF.
  MESSAGE S009(ZMS_COL_LIB).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0300_EXECUTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0300_EXECUTE .

  PERFORM CONVERT_FS_CELLS_TO_DATA
    USING GS_RPC_IN_F.

*  BREAK CITEK_DEV.
  CALL FUNCTION 'ZFM_BM_RPC_EXECUTE_FORM'
    EXPORTING
      I_LANGU   = ZST0_BM_RPC_DIMSET-SPRAS
    CHANGING
      C_FORMSTR = GS_RPC_IN_F.

  PERFORM FORM_GEN_DISPLAY_CELLS.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0310_ADD_ROWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0310_ADD_ROWS .
  DATA:
    LS_DATA    TYPE ZST0_BM_RPC_D.

  CLEAR: LS_DATA.
  LS_DATA-ROWPOS  = LINES( GS_RPC_IN_F-DATA ) + 1.
  APPEND LS_DATA TO GS_RPC_IN_F-DATA.

  PERFORM FORM_GEN_DISPLAY_CELLS.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CELL_KEYFIGURE_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LPS_DIMENSION  text
*      -->LPS_SEGITM  text
*----------------------------------------------------------------------*
FORM CELL_KEYFIGURE_GET_DATA
  USING LPS_RPC_F     TYPE ZST0_BM_RPC_F
        LPS_DIMENSION TYPE ZST0_BM_RPC_DIMSET
  CHANGING LPW_CELL_DATA .
  DATA:
    LT_SICOND     TYPE ZTT0_BM_RPC_SICOND,
    LT_CONDITIONS TYPE ZTT_BM_FIELD_COND,
    LS_CONDITION  TYPE ZST_BM_FIELD_COND,
    LT_COND_SET   TYPE ZTT_BM_COND_SET,
    LT_WHERE      TYPE RSDS_TWHERE,
    LS_WHERE      TYPE RSDS_WHERE,
    LS_WHERE_A    TYPE RSDS_WHERE,
    LW_SELSTR     TYPE STRING,
    LW_CELL       TYPE ACDOCA-HSL,
    LW_AGGR       TYPE CHAR50.

  LT_SICOND = LPS_RPC_F-SEGITM-CONDITIONS.
  DELETE LT_SICOND
    WHERE ROWNO <> LPS_DIMENSION-ROWNO
      AND COLNO <> LPS_DIMENSION-COLNO.

  LOOP AT LT_SICOND INTO DATA(LS_SICOND)
    WHERE RSIGN IS NOT INITIAL.
    CLEAR: LS_CONDITION.
    PERFORM CONDITION_CONVERSION_EXIT
      CHANGING LS_SICOND.

    MOVE-CORRESPONDING LS_SICOND TO LS_CONDITION.
    READ TABLE GT_RPC_CHAR INTO DATA(LS_CHAR)
      WITH KEY TABNAME    = LS_CONDITION-RTABLE
               FIELDNAME  = LS_CONDITION-RFIELD BINARY SEARCH.
    IF SY-SUBRC IS INITIAL AND LS_CHAR-CHECKTABLE IS NOT INITIAL.
      PERFORM CONDITION_GET_VARIABLE_VALUE
        USING LPS_RPC_F
              LS_CHAR
        CHANGING LS_CONDITION.
    ENDIF.
    APPEND LS_CONDITION TO LT_CONDITIONS.
  ENDLOOP.

  CALL FUNCTION 'ZFM_DATA_COND_AGG_TO_SET'
    EXPORTING
      T_CONDITIONS = LT_CONDITIONS
    IMPORTING
      T_COND_SET   = LT_COND_SET.

  IF LPS_DIMENSION-AGGFUNC IS INITIAL.
    LPS_DIMENSION-AGGFUNC = 'SUM'.
    LW_AGGR = 'SUM( '.
  ELSEIF LPS_DIMENSION-AGGFUNC = 'COUNT'.
    LW_AGGR = 'COUNT( DISTINCT '.
    .
  ELSE.
    LW_AGGR = LPS_DIMENSION-AGGFUNC && '( '.
  ENDIF.

  CONCATENATE LW_AGGR LPS_DIMENSION-KEYFIG  ' )'
         INTO LW_SELSTR SEPARATED BY SPACE.

  LOOP AT LT_COND_SET INTO DATA(LS_COND_SET).
    IF LS_WHERE_A-WHERE_TAB IS NOT INITIAL.
      APPEND 'OR' TO LS_WHERE_A-WHERE_TAB.
    ENDIF.

    CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_WHERE'
      EXPORTING
        FIELD_RANGES  = LS_COND_SET-TAB_COND
      IMPORTING
        WHERE_CLAUSES = LT_WHERE.

    READ TABLE LT_WHERE INTO LS_WHERE
      WITH KEY TABLENAME = LPS_DIMENSION-SRCTAB.
    IF SY-SUBRC IS INITIAL.
      APPEND '(' TO LS_WHERE_A-WHERE_TAB.
      APPEND LINES OF LS_WHERE-WHERE_TAB TO LS_WHERE_A-WHERE_TAB.
      APPEND ')' TO LS_WHERE_A-WHERE_TAB.
    ENDIF.
  ENDLOOP.

  IF LS_WHERE_A-WHERE_TAB IS NOT INITIAL.
    SELECT (LW_SELSTR)
      FROM (LPS_DIMENSION-SRCTAB)
      INTO LW_CELL "LPW_CELL_DATA
     WHERE (LS_WHERE_A-WHERE_TAB).
      IF LPS_DIMENSION-REVERSE IS NOT INITIAL.
        LW_CELL = - LW_CELL.
      ENDIF.

      IF LPS_DIMENSION-AGGFUNC = 'SUM'.
        WRITE LW_CELL TO LPW_CELL_DATA CURRENCY 'VND' NO-GROUPING.
      ELSE.
        WRITE LW_CELL TO LPW_CELL_DATA NO-GROUPING DECIMALS 0.
      ENDIF.

      WRITE LW_CELL TO LPW_CELL_DATA CURRENCY 'VND' NO-GROUPING.
      CONDENSE LPW_CELL_DATA.

      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          VALUE = LPW_CELL_DATA.
    ENDSELECT.
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CELL_FORMULA_CALCULATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LPS_DIMENSION  text
*      -->LPT_DATA  text
*----------------------------------------------------------------------*
FORM CELL_FORMULA_CALCULATE
  USING LPS_DIMENSION TYPE ZST0_BM_RPC_DIMSET
        LPT_DATA      TYPE ZTT0_BM_RPC_D
  CHANGING LPW_CELL_DATA ..


  DATA :
    LW_ACTOR      TYPE ZDD_BM_AGGEX,
    LW_ACTOR_TMP  TYPE ZDD_BM_AGGEX,
    LT_ACTOR      LIKE TABLE OF LW_ACTOR,
    LT_ACTOR_TMP  LIKE TABLE OF LW_ACTOR,
    LT_ACTOR_LOOP LIKE TABLE OF LW_ACTOR,
    LT_SEPARATOR  TYPE TABLE OF C,
    LW_SEPARATOR  TYPE C,
*    LW_AGGFIG     TYPE FLOAT,
    LW_AGGFIG     TYPE DECV15,
    LW_ROWNO      TYPE NUMC3,
    LW_COLNO      TYPE NUMC2,
    LW_FIELDNAME  TYPE FIELDNAME.
  FIELD-SYMBOLS:
    <LF_ACTOR>       TYPE ZDD_BM_AGGEX,
    <LF_ACTOR_FIELD> TYPE ANY.

* Prepare separators
  APPEND '(' TO LT_SEPARATOR.
  APPEND ')' TO LT_SEPARATOR.
  APPEND '/' TO LT_SEPARATOR.
  APPEND '+' TO LT_SEPARATOR.
  APPEND '*' TO LT_SEPARATOR.
  APPEND '-' TO LT_SEPARATOR.

* Prepare actors
  APPEND LPS_DIMENSION-FORMULA TO LT_ACTOR.

* Separate all minimum actors
  LOOP AT LT_SEPARATOR INTO LW_SEPARATOR.
    CLEAR: LT_ACTOR_LOOP.
    LOOP AT LT_ACTOR INTO LW_ACTOR.
      CLEAR: LT_ACTOR_TMP.
      IF LW_ACTOR CS LW_SEPARATOR.
        SPLIT LW_ACTOR AT LW_SEPARATOR INTO TABLE LT_ACTOR_TMP.
        LOOP AT LT_ACTOR_TMP INTO LW_ACTOR_TMP.
          CONDENSE LW_ACTOR_TMP.
          IF SY-TABIX <> 1.
            APPEND LW_SEPARATOR TO LT_ACTOR_LOOP.
          ENDIF.
          APPEND LW_ACTOR_TMP TO LT_ACTOR_LOOP.
        ENDLOOP.
        IF LINES( LT_ACTOR_TMP ) = 1.
          APPEND LW_SEPARATOR TO LT_ACTOR_LOOP.
        ENDIF.
      ELSE.
        APPEND LW_ACTOR TO LT_ACTOR_LOOP.
      ENDIF.
    ENDLOOP.
    LT_ACTOR = LT_ACTOR_LOOP.
  ENDLOOP.

* Replace fieldname in actor with it's value
  LOOP AT LT_ACTOR ASSIGNING <LF_ACTOR>.
    IF <LF_ACTOR>(1) = 'X'.
      LW_ROWNO = <LF_ACTOR>+1.
      READ TABLE LPT_DATA INTO DATA(LS_DATA) INDEX LW_ROWNO.
      IF SY-SUBRC IS INITIAL.
        LW_FIELDNAME = 'COL' && LPS_DIMENSION-COLNO.
        ASSIGN COMPONENT LW_FIELDNAME OF STRUCTURE LS_DATA
          TO <LF_ACTOR_FIELD>.
        IF SY-SUBRC IS INITIAL.
          <LF_ACTOR> = <LF_ACTOR_FIELD>.
          CONDENSE <LF_ACTOR>.
        ELSE.
          MESSAGE A001(ZMS_COL_LIB) WITH <LF_ACTOR>+1 LPS_DIMENSION-FORMULA.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'CATS_NUMERIC_INPUT_CHECK'
        EXPORTING
          INPUT      = <LF_ACTOR>
        EXCEPTIONS
          NO_NUMERIC = 1
          OTHERS     = 2.
      IF SY-SUBRC IS INITIAL.
        CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
          CHANGING
            VALUE = <LF_ACTOR>.
        IF <LF_ACTOR> < 0.
          <LF_ACTOR> = '(' && <LF_ACTOR> && ')'.
        ENDIF.
      ENDIF.

    ELSEIF <LF_ACTOR>(1) = 'Y'.
      LW_COLNO = <LF_ACTOR>+1.
      READ TABLE LPT_DATA INTO LS_DATA INDEX LPS_DIMENSION-ROWNO.
      IF SY-SUBRC IS INITIAL.
        LW_FIELDNAME = 'COL' && LW_COLNO.
        ASSIGN COMPONENT LW_FIELDNAME OF STRUCTURE LS_DATA
          TO <LF_ACTOR_FIELD>.
        IF SY-SUBRC IS INITIAL.
          <LF_ACTOR> = <LF_ACTOR_FIELD>.
          CONDENSE <LF_ACTOR>.
        ELSE.
          MESSAGE A001(ZMS_COL_LIB) WITH <LF_ACTOR>+1 LPS_DIMENSION-FORMULA.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'CATS_NUMERIC_INPUT_CHECK'
        EXPORTING
          INPUT      = <LF_ACTOR>
        EXCEPTIONS
          NO_NUMERIC = 1
          OTHERS     = 2.
      IF SY-SUBRC IS INITIAL.
        CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
          CHANGING
            VALUE = <LF_ACTOR>.
        IF <LF_ACTOR> < 0.
          <LF_ACTOR> = '(' && <LF_ACTOR> && ')'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

* Rebuld expression
  CONCATENATE LINES OF LT_ACTOR INTO LPS_DIMENSION-FORMULA SEPARATED BY SPACE.

* Calculate expression
  CALL FUNCTION 'EVAL_FORMULA'
    EXPORTING
      FORMULA                 = LPS_DIMENSION-FORMULA
    IMPORTING
      VALUE                   = LW_AGGFIG
    EXCEPTIONS
      DIVISION_BY_ZERO        = 1
      EXP_ERROR               = 2
      FORMULA_TABLE_NOT_VALID = 3
      INVALID_EXPRESSION      = 4
      INVALID_VALUE           = 5
      LOG_ERROR               = 6
      PARAMETER_ERROR         = 7
      SQRT_ERROR              = 8
      UNITS_NOT_VALID         = 9
      MISSING_PARAMETER       = 10
      OTHERS                  = 11.
  IF SY-SUBRC IS INITIAL.
    WRITE LW_AGGFIG TO LPW_CELL_DATA NO-GROUPING.
    CONDENSE LPW_CELL_DATA.
    CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
      CHANGING
        VALUE = LPW_CELL_DATA.
  ELSE.
    IF GW_NO_SHOW_ERR IS INITIAL.
      LPW_CELL_DATA = TEXT-007 && LPS_DIMENSION-FORMULA.
    ELSE.
      CLEAR: LPW_CELL_DATA.
    ENDIF.
  ENDIF.
*  LPW_CELL_DATA   = LW_AGGFIG.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  INIT_F_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_F_DATA
  CHANGING C_FORMSTR  TYPE ZST0_BM_RPC_F.
  DATA:
    LS_DATA   TYPE ZST0_BM_RPC_D.

  LOOP AT C_FORMSTR-SEGITM-ROWS INTO DATA(LS_SIR)
    WHERE HROW IS INITIAL.
    READ TABLE C_FORMSTR-DATA TRANSPORTING NO FIELDS
      WITH KEY ROWPOS = LS_SIR-ROWNO.
    IF SY-SUBRC IS NOT INITIAL.
      CLEAR: LS_DATA.
      MOVE-CORRESPONDING LS_SIR TO LS_DATA.
      LS_DATA-LANGU = SPACE.
      LS_DATA-ROWPOS = LS_SIR-ROWNO.
      APPEND LS_DATA TO C_FORMSTR-DATA.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FORM_OUT_GEN_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LPS_RPC_R  text
*      -->LPS_RPC_S  text
*      -->LPS_RPC_SI  text
*      <--LPS_DATA_SITM  text
*----------------------------------------------------------------------*
FORM FORM_OUT_GEN_LINE
  USING   LPS_RPC_R     TYPE ZST0_BM_RPC_R
          LPS_RPC_S     TYPE ZST0_BM_RPC_S
          LPS_RPC_SI    TYPE ZST0_BM_RPC_SI
 CHANGING LPS_DATA_SITM TYPE ZST1_BM_RPC_SI.

  DATA:
    LT_ITM_TAB TYPE TABLE OF ZTB_BM_RPC_D,
    LS_ITM_TAB TYPE ZTB_BM_RPC_D.
  FIELD-SYMBOLS:
    <LF_COLVAL>  TYPE ANY.

  CLEAR: LS_ITM_TAB.
  MOVE-CORRESPONDING LPS_RPC_R TO LS_ITM_TAB.
  MOVE-CORRESPONDING LPS_RPC_S TO LS_ITM_TAB.
  MOVE-CORRESPONDING LPS_RPC_SI TO LS_ITM_TAB.
  IF LS_ITM_TAB-STYLE1 = SPACE
  AND LS_ITM_TAB-STYLE2 = SPACE
  AND LS_ITM_TAB-STYLE3 = SPACE.
    LS_ITM_TAB-STYLE1 = 'X'.
  ENDIF.
  LS_ITM_TAB-EFMON = GS_RPC_R-MONTH.

  LOOP AT LPS_RPC_SI-FIGURES INTO DATA(LS_RPC_LF).
    CASE LS_RPC_LF-SRCDT.
      WHEN GC_SRCDT_FIXVAL.
        ASSIGN COMPONENT LS_RPC_LF-FNAME OF STRUCTURE LS_ITM_TAB
          TO <LF_COLVAL>.
        IF SY-SUBRC IS INITIAL.
          <LF_COLVAL> = LS_RPC_LF-FXVAL.
          CONDENSE <LF_COLVAL>.
        ENDIF.
      WHEN GC_SRCDT_FUNCMD.
        CALL FUNCTION 'FUNCTION_PRUEF_TRDIR1'
          EXPORTING
            IM_FUNCNAME  = LS_RPC_LF-FUNCN
          EXCEPTIONS
            FB_NOT_VALID = 1
            OTHERS       = 2.
        CHECK SY-SUBRC IS INITIAL.

        CALL FUNCTION LS_RPC_LF-FUNCN "'ZFM_RPCF_HPA_1100'
          EXPORTING
            I_MONTH   = GS_RPC_R-MONTH
            IS_RPC_LF = LS_RPC_LF
            IS_RPC_SI = LPS_RPC_SI
          CHANGING
            CS_RPC_D  = LS_ITM_TAB
            CT_RPC_D  = LT_ITM_TAB.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

* Set template to show
  PERFORM SEGMENT_ITEM_SET_TEMPL_DATA
    USING LPS_RPC_SI-TEMPL
          LS_ITM_TAB
    CHANGING LPS_DATA_SITM.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FORM_OUT_GEN_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LPS_RPC_R  text
*      -->LPS_RPC_S  text
*      -->LPS_RPC_SI  text
*      <--LPS_DATA_SITM  text
*----------------------------------------------------------------------*
FORM FORM_OUT_GEN_TAB
  USING   LPS_RPC_R     TYPE ZST0_BM_RPC_R
          LPS_RPC_S     TYPE ZST0_BM_RPC_S
          LPS_RPC_SI    TYPE ZST0_BM_RPC_SI
 CHANGING LPS_DATA_SITM TYPE ZST1_BM_RPC_SI.

  DATA:
    LT_ITM_TAB TYPE TABLE OF ZTB_BM_RPC_D,
    LS_ITM_TAB TYPE ZTB_BM_RPC_D.
  FIELD-SYMBOLS:
    <LF_COLVAL>  TYPE ANY.


  CLEAR: LT_ITM_TAB, LS_ITM_TAB.
  READ TABLE LPS_RPC_R-FORMSTR INTO DATA(LS_RPC_F)
    WITH KEY RPSEG = LPS_RPC_S-RPSEG
             RITEM = LPS_RPC_SI-RITEM BINARY SEARCH.
  IF SY-SUBRC IS INITIAL.
    LS_RPC_F-SEGITM = LPS_RPC_SI.
    CALL FUNCTION 'ZFM_BM_RPC_EXECUTE_FORM'
      EXPORTING
        I_LANGU   = LPS_RPC_R-LANGU
      CHANGING
        C_FORMSTR = LS_RPC_F.

*   Set template to show
    PERFORM SEGMENT_ITEM_SET_TEMPL_TABLE
      USING LS_RPC_F
            LPS_RPC_R-LANGU
      CHANGING LPS_DATA_SITM.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FORM_OUT_GEN_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LPS_RPC_R  text
*      -->LPS_RPC_S  text
*      -->LPS_RPC_SI  text
*      <--LPS_DATA_SITM  text
*----------------------------------------------------------------------*
FORM FORM_OUT_GEN_TEXT
  USING   LPS_RPC_R     TYPE ZST0_BM_RPC_R
          LPS_RPC_S     TYPE ZST0_BM_RPC_S
          LPS_RPC_SI    TYPE ZST0_BM_RPC_SI
 CHANGING LPS_DATA_SITM TYPE ZST1_BM_RPC_SI.

  DATA:
    LT_ITM_TAB TYPE TABLE OF ZTB_BM_RPC_D,
    LS_ITM_TAB TYPE ZTB_BM_RPC_D.
  FIELD-SYMBOLS:
    <LF_COLVAL>  TYPE ANY.


  CLEAR: LT_ITM_TAB, LS_ITM_TAB.
  READ TABLE LPS_RPC_R-FORMSTR INTO DATA(LS_RPC_F)
    WITH KEY RPSEG = LPS_RPC_S-RPSEG
             RITEM = LPS_RPC_SI-RITEM BINARY SEARCH.
  IF SY-SUBRC IS INITIAL.
    LS_RPC_F-SEGITM = LPS_RPC_SI.

*   Set template to show
    PERFORM SEGMENT_ITEM_SET_TEMPL_TEXT
      USING LPS_RPC_R
            LS_RPC_F
      CHANGING LPS_DATA_SITM.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FORM_OUT_GEN_FSV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LPS_RPC_R  text
*      -->LPS_RPC_S  text
*      -->LPS_RPC_SI  text
*      <--LPS_DATA_SITM  text
*----------------------------------------------------------------------*
FORM FORM_OUT_GEN_FSV
  USING   LPS_RPC_R     TYPE ZST0_BM_RPC_R
          LPS_RPC_S     TYPE ZST0_BM_RPC_S
          LPS_RPC_SI    TYPE ZST0_BM_RPC_SI
 CHANGING LPS_DATA_SITM TYPE ZST1_BM_RPC_SI.
  DATA:
    LW_PYEAR     TYPE GJAHR,
    LR_PERI      TYPE RANGE OF ACDOCA-POPER,
    LR_CPERI     TYPE RANGE OF ACDOCA-POPER,
    LS_PERI      LIKE LINE OF LR_PERI,
    LT_FSV_ITEMS TYPE ZTT_BM_RPC_FSV_D,
    LW_EN        TYPE XMARK,
    LW_VI        TYPE XMARK.

  LW_PYEAR = LPS_RPC_R-MONTH(4) - 1.

  LS_PERI-SIGN    = 'I'.
  LS_PERI-OPTION  = 'BT'.
  LS_PERI-LOW     = 001.
  LS_PERI-HIGH    = LPS_RPC_R-MONTH+4.
  APPEND LS_PERI TO LR_PERI.

  IF LPS_RPC_R-LANGU = 'E'.
    LW_EN = 'X'.
    LW_VI = SPACE.
  ELSE.
    LW_VI = 'X'.
    LW_EN = SPACE.
  ENDIF.

  CASE LPS_RPC_SI-VERSN.
    WHEN 'ZBS1'.
      SUBMIT ZPG_FSV001 AND RETURN
        WITH P_YEAR   EQ LPS_RPC_R-MONTH(4)
        WITH S_PERI   IN LR_PERI
        WITH P_CYEAR  EQ LW_PYEAR
        WITH P_EN     EQ LW_EN
        WITH P_VI     EQ LW_VI
        WITH P_EXDATA EQ 'X'
        .

      IMPORT FSV_DATA = LT_FSV_ITEMS FROM MEMORY ID LPS_RPC_SI-VERSN.
      FREE MEMORY ID LPS_RPC_SI-VERSN.

    WHEN 'ZPL1' OR 'ZPL2'.
      SUBMIT ZRFSV001 AND RETURN
        WITH P_VERSN  EQ LPS_RPC_SI-VERSN
        WITH P_YEAR   EQ LPS_RPC_R-MONTH(4)
        WITH S_PERI   IN LR_PERI
        WITH P_CYEAR  EQ LW_PYEAR
        WITH S_CPERI  IN LR_PERI
        WITH P_EN     EQ LW_EN
        WITH P_VI     EQ LW_VI
        WITH P_EXDATA EQ 'X'.

      IMPORT FSV_DATA = LT_FSV_ITEMS FROM MEMORY ID LPS_RPC_SI-VERSN.
      FREE MEMORY ID LPS_RPC_SI-VERSN.
    WHEN 'ZCF2'.
      SUBMIT ZRFSV004 AND RETURN
        WITH P_VERSN  EQ LPS_RPC_SI-VERSN
        WITH P_YEAR   EQ LPS_RPC_R-MONTH(4)
        WITH S_PERI   IN LR_PERI
        WITH P_CYEAR  EQ LW_PYEAR
        WITH S_CPERI  IN LR_PERI
        WITH P_EN     EQ LW_EN
        WITH P_VI     EQ LW_VI
        WITH P_EXDATA EQ 'X'.

      IMPORT FSV_DATA = LT_FSV_ITEMS FROM MEMORY ID LPS_RPC_SI-VERSN.
      FREE MEMORY ID LPS_RPC_SI-VERSN.
    WHEN OTHERS.
  ENDCASE.


* Set template to show
  PERFORM SEGMENT_ITEM_SET_TEMPL_FSV
    USING LPS_RPC_SI
          LT_FSV_ITEMS
    CHANGING LPS_DATA_SITM.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GEN_FSV_LINE_4_COL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LPS_FSV_ITEM  text
*      <--LPS_DATA_EXC  text
*----------------------------------------------------------------------*
FORM GEN_FSV_LINE_4_COL
  USING    LPS_FSV_ITEM TYPE ZST_BM_RPC_FSV_D
           LPW_BOLDLV   TYPE I
  CHANGING LPS_DATA_EXC TYPE ZST0_BM_RPC_D.

  CLEAR: LPS_DATA_EXC.
  LPS_DATA_EXC-CEL01-CELLVAL = LPS_FSV_ITEM-CODE.
  IF LPS_FSV_ITEM-STUFE <= LPW_BOLDLV.
    LPS_DATA_EXC-CEL01-CSTLB = 'X'.
  ELSE.
    LPS_DATA_EXC-CEL01-CSTLN = 'X'.
  ENDIF.

  LPS_DATA_EXC-CEL02-CELLVAL = LPS_FSV_ITEM-NAME.
  IF LPS_FSV_ITEM-STUFE <= LPW_BOLDLV.
    LPS_DATA_EXC-CEL02-CSTLB = 'X'.
  ELSE.
    LPS_DATA_EXC-CEL02-CSTLN = 'X'.
  ENDIF.

  LPS_DATA_EXC-CEL03-CELLVAL = LPS_FSV_ITEM-HSL.
  WRITE LPS_FSV_ITEM-HSL TO LPS_DATA_EXC-CEL03-CELLVAL
    NO-GROUPING DECIMALS 0 CURRENCY 'VND'.
  CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
    CHANGING
      VALUE = LPS_DATA_EXC-CEL03-CELLVAL.

  CONDENSE LPS_DATA_EXC-CEL03-CELLVAL.
  IF LPS_FSV_ITEM-STUFE <= LPW_BOLDLV.
    LPS_DATA_EXC-CEL03-CSTLB = 'X'.
  ELSE.
    LPS_DATA_EXC-CEL03-CSTLN = 'X'.
  ENDIF.

  LPS_DATA_EXC-CEL04-CELLVAL = LPS_FSV_ITEM-HSLC.
  WRITE LPS_FSV_ITEM-HSLC TO LPS_DATA_EXC-CEL04-CELLVAL
    NO-GROUPING DECIMALS 0 CURRENCY 'VND'.
  CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
    CHANGING
      VALUE = LPS_DATA_EXC-CEL04-CELLVAL.
  CONDENSE LPS_DATA_EXC-CEL04-CELLVAL.
  IF LPS_FSV_ITEM-STUFE <= LPW_BOLDLV.
    LPS_DATA_EXC-CEL04-CSTLB = 'X'.
  ELSE.
    LPS_DATA_EXC-CEL04-CSTLN = 'X'.
  ENDIF.

* CSTLI CSTLU CSTLD
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GEN_FSV_LINE_5_COL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LPS_FSV_ITEM  text
*      <--LPS_DATA_EXC  text
*----------------------------------------------------------------------*
FORM GEN_FSV_LINE_5_COL
  USING    LPS_FSV_ITEM TYPE ZST_BM_RPC_FSV_D
           LPW_BOLDLV   TYPE I
  CHANGING LPS_DATA_EXC TYPE ZST0_BM_RPC_D.

  CLEAR: LPS_DATA_EXC.
  LPS_DATA_EXC-CEL01-CELLVAL = LPS_FSV_ITEM-CODE.
  IF LPS_FSV_ITEM-STUFE <= LPW_BOLDLV.
    LPS_DATA_EXC-CEL01-CSTLB = 'X'.
  ELSE.
    LPS_DATA_EXC-CEL01-CSTLN = 'X'.
  ENDIF.

  LPS_DATA_EXC-CEL02-CELLVAL = LPS_FSV_ITEM-NAME.
  IF LPS_FSV_ITEM-STUFE <= LPW_BOLDLV.
    LPS_DATA_EXC-CEL02-CSTLB = 'X'.
  ELSE.
    LPS_DATA_EXC-CEL02-CSTLN = 'X'.
  ENDIF.

  LPS_DATA_EXC-CEL03-CELLVAL = LPS_FSV_ITEM-DESC.
  IF LPS_FSV_ITEM-STUFE <= LPW_BOLDLV.
    LPS_DATA_EXC-CEL03-CSTLB = 'X'.
  ELSE.
    LPS_DATA_EXC-CEL03-CSTLN = 'X'.
  ENDIF.

  LPS_DATA_EXC-CEL04-CELLVAL = LPS_FSV_ITEM-HSL.
  WRITE LPS_FSV_ITEM-HSL TO LPS_DATA_EXC-CEL04-CELLVAL
    NO-GROUPING DECIMALS 0 CURRENCY 'VND'.
  CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
    CHANGING
      VALUE = LPS_DATA_EXC-CEL04-CELLVAL.
  CONDENSE LPS_DATA_EXC-CEL04-CELLVAL.
  IF LPS_FSV_ITEM-STUFE <= LPW_BOLDLV.
    LPS_DATA_EXC-CEL04-CSTLB = 'X'.
  ELSE.
    LPS_DATA_EXC-CEL04-CSTLN = 'X'.
  ENDIF.

  LPS_DATA_EXC-CEL05-CELLVAL = LPS_FSV_ITEM-HSLC.
  WRITE LPS_FSV_ITEM-HSLC TO LPS_DATA_EXC-CEL05-CELLVAL
    NO-GROUPING DECIMALS 0 CURRENCY 'VND'.
  CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
    CHANGING
      VALUE = LPS_DATA_EXC-CEL05-CELLVAL.
  CONDENSE LPS_DATA_EXC-CEL05-CELLVAL.
  CONDENSE LPS_DATA_EXC-CEL05-CELLVAL.
  IF LPS_FSV_ITEM-STUFE <= LPW_BOLDLV.
    LPS_DATA_EXC-CEL05-CSTLB = 'X'.
  ELSE.
    LPS_DATA_EXC-CEL05-CSTLN = 'X'.
  ENDIF.

* CSTLI CSTLU CSTLD
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CONVERT_CELL_STYLE_TO_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LPW_CSTYLE  text
*      <--ZST0_BM_RPC_CELLOUTPUT  text
*----------------------------------------------------------------------*
FORM CONVERT_CELL_STYLE_TO_OUTPUT
  USING    LPW_CSTYLE TYPE ZDD_BM_RPC_CSTYLE
  CHANGING LPS_CELLOUT TYPE ZST0_BM_RPC_CELLOUTPUT.
  DATA:
    LS_CSTYLE    TYPE ZST_BM_RPC_CSTYLE,
    LW_FIELDNAME TYPE FIELDNAME.

  CLEAR: LS_CSTYLE.
  LW_FIELDNAME = 'CSTL' && LPW_CSTYLE.

  ASSIGN COMPONENT LW_FIELDNAME OF STRUCTURE LPS_CELLOUT
    TO FIELD-SYMBOL(<LF_CSTYLE>).
  IF SY-SUBRC IS INITIAL.
    MOVE-CORRESPONDING LS_CSTYLE TO LPS_CELLOUT.
    <LF_CSTYLE> = 'X'.
  ENDIF.

*  MOVE-CORRESPONDING LPS_CELLOUT TO LS_CSTYLE.
*  IF LS_CSTYLE IS INITIAL.
*    LS_CSTYLE-CSTLN = 'X'.
*    MOVE-CORRESPONDING LS_CSTYLE TO LPS_CELLOUT.
*  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_FORM_ALV_HOSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  E_COLUMN_ID        text
*  -->  ES_ROW_NO        text
*----------------------------------------------------------------------*
FORM HANDLE_FORM_ALV_HOSPOT_CLICK
  USING E_COLUMN_ID	TYPE LVC_S_COL
        ES_ROW_NO	TYPE LVC_S_ROID.
  DATA:
    LS_TDTEXT TYPE EFG_STRN_TDTEXT.

  CHECK GS_RPC_IN_F-SEGITM-ITMTY = GC_ITEMTYPE_TEXT
    AND ( E_COLUMN_ID-FIELDNAME = 'TEX01' OR E_COLUMN_ID-FIELDNAME = 'TEX02' ).

  READ TABLE GS_RPC_IN_F-CELLS ASSIGNING FIELD-SYMBOL(<LF_CELL>)
    INDEX ES_ROW_NO-ROW_ID.
  CHECK SY-SUBRC IS INITIAL.
  LS_TDTEXT-TDNAME = ZVI_BM_RPC_F-REPID && ZVI_BM_RPC_F-EFMON
                  && ZVI_BM_RPC_F-RPSEG  && ZVI_BM_RPC_F-RITEM
                  && <LF_CELL>-ROWNO.

  IF E_COLUMN_ID-FIELDNAME = 'TEX01'.
    LS_TDTEXT-TDSPRAS = 'E'.
  ELSEIF E_COLUMN_ID-FIELDNAME = 'TEX02'.
    LS_TDTEXT-TDSPRAS = GC_LANGU_VN.
  ENDIF.

  READ TABLE GS_RPC_IN_F-TEXTS INTO LS_TDTEXT
    WITH KEY TDNAME   = LS_TDTEXT-TDNAME
             TDSPRAS  = LS_TDTEXT-TDSPRAS.
  IF SY-SUBRC IS NOT INITIAL.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        ID                      = 'Z001'
        LANGUAGE                = LS_TDTEXT-TDSPRAS
        NAME                    = LS_TDTEXT-TDNAME
        OBJECT                  = 'ZRPC'
      IMPORTING
        HEADER                  = LS_TDTEXT-THEAD
      TABLES
        LINES                   = LS_TDTEXT-T_LINES
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        OTHERS                  = 8.
    IF SY-SUBRC = 4.
      LS_TDTEXT-THEAD-TDID                  = 'Z001'.
      LS_TDTEXT-THEAD-TDSPRAS              = LS_TDTEXT-TDSPRAS.
      LS_TDTEXT-THEAD-TDNAME               = LS_TDTEXT-TDNAME.
      LS_TDTEXT-THEAD-TDOBJECT             = 'ZRPC'.
      LS_TDTEXT-THEAD-TDLINESIZE           = 132.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'EDIT_TEXT'
    EXPORTING
      HEADER        = LS_TDTEXT-THEAD
      SAVE          = 'X'
    TABLES
      LINES         = LS_TDTEXT-T_LINES
    EXCEPTIONS
      ID            = 1
      LANGUAGE      = 2
      LINESIZE      = 3
      NAME          = 4
      OBJECT        = 5
      TEXTFORMAT    = 6
      COMMUNICATION = 7
      OTHERS        = 8.

  READ TABLE GS_RPC_IN_F-TEXTS ASSIGNING FIELD-SYMBOL(<LF_TDTEXT>)
    WITH KEY TDNAME   = LS_TDTEXT-TDNAME
             TDSPRAS  = LS_TDTEXT-TDSPRAS.
  IF SY-SUBRC IS INITIAL.
    <LF_TDTEXT> = LS_TDTEXT.
  ELSE.
    APPEND LS_TDTEXT TO GS_RPC_IN_F-TEXTS.
  ENDIF.


  READ TABLE LS_TDTEXT-T_LINES INTO DATA(LS_LINE) INDEX 1.

  READ TABLE GS_RPC_IN_F-DATA ASSIGNING FIELD-SYMBOL(<LF_DATA>)
    INDEX ES_ROW_NO-ROW_ID.
  CHECK SY-SUBRC IS INITIAL.

  IF E_COLUMN_ID-FIELDNAME = 'TEX01'.
    <LF_DATA>-TEX01 = <LF_CELL>-TEX01 = LS_LINE-TDLINE.
  ELSE.
    <LF_DATA>-TEX02 = <LF_CELL>-TEX02 = LS_LINE-TDLINE.
  ENDIF.

  CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
    EXPORTING
      I_ALV_GRID = GO_F_ALV.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_FORM_320_HOSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  E_COLUMN_ID        text
*  -->  ES_ROW_NO        text
*----------------------------------------------------------------------*
FORM HANDLE_FORM_320_HOSPOT_CLICK
  USING E_COLUMN_ID	TYPE LVC_S_COL
        ES_ROW_NO	TYPE LVC_S_ROID.
  DATA:
    LS_TDTEXT TYPE EFG_STRN_TDTEXT,
    LT_STREAM TYPE TABLE OF TEXT132.

  PERFORM 0320_UPDATE_TEXT_TO_ALV.

  CHECK GS_RPC_IN_F-SEGITM-ITMTY = GC_ITEMTYPE_TEXT
    AND ( E_COLUMN_ID-FIELDNAME = 'TEX01' OR E_COLUMN_ID-FIELDNAME = 'TEX02' ).
  CLEAR: ZST0_BM_RPC_DIMSET.

  READ TABLE GS_RPC_IN_F-CELLS ASSIGNING FIELD-SYMBOL(<LF_CELL>)
    INDEX ES_ROW_NO-ROW_ID.
  CHECK SY-SUBRC IS INITIAL.
  LS_TDTEXT-TDNAME = ZVI_BM_RPC_F-REPID && ZVI_BM_RPC_F-EFMON
                  && ZVI_BM_RPC_F-RPSEG  && ZVI_BM_RPC_F-RITEM
                  && <LF_CELL>-ROWNO.
  ZST0_BM_RPC_DIMSET-ROWNO  = <LF_CELL>-ROWNO.
  ZST0_BM_RPC_DIMSET-CSTYLE = <LF_CELL>-LSTYLE(1).
  IF E_COLUMN_ID-FIELDNAME  = 'TEX01'.
    ZST0_BM_RPC_DIMSET-SPRAS = LS_TDTEXT-TDSPRAS = 'E'.
  ELSEIF E_COLUMN_ID-FIELDNAME = 'TEX02'.
    ZST0_BM_RPC_DIMSET-SPRAS = LS_TDTEXT-TDSPRAS = GC_LANGU_VN.
  ENDIF.

  READ TABLE GS_RPC_IN_F-TEXTS INTO LS_TDTEXT
    WITH KEY TDNAME   = LS_TDTEXT-TDNAME
             TDSPRAS  = LS_TDTEXT-TDSPRAS.
  IF SY-SUBRC IS NOT INITIAL.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        ID                      = 'Z001'
        LANGUAGE                = LS_TDTEXT-TDSPRAS
        NAME                    = LS_TDTEXT-TDNAME
        OBJECT                  = 'ZRPC'
      IMPORTING
        HEADER                  = LS_TDTEXT-THEAD
      TABLES
        LINES                   = LS_TDTEXT-T_LINES
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        OTHERS                  = 8.
    IF SY-SUBRC = 4.
      LS_TDTEXT-THEAD-TDID                  = 'Z001'.
      LS_TDTEXT-THEAD-TDSPRAS              = LS_TDTEXT-TDSPRAS.
      LS_TDTEXT-THEAD-TDNAME               = LS_TDTEXT-TDNAME.
      LS_TDTEXT-THEAD-TDOBJECT             = 'ZRPC'.
      LS_TDTEXT-THEAD-TDLINESIZE           = 132.
    ENDIF.
    APPEND LS_TDTEXT TO GS_RPC_IN_F-TEXTS.
  ENDIF.

  CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
    EXPORTING
      LANGUAGE    = ZST0_BM_RPC_DIMSET-SPRAS
    TABLES
      ITF_TEXT    = LS_TDTEXT-T_LINES
      TEXT_STREAM = LT_STREAM.


  CALL METHOD GO_F_TEXT->SET_TEXT_AS_STREAM
    EXPORTING
      TEXT            = LT_STREAM
    EXCEPTIONS
      ERROR_DP        = 1
      ERROR_DP_CREATE = 2
      OTHERS          = 3.

  CALL METHOD CL_GUI_CFW=>SET_NEW_OK_CODE
    EXPORTING
      NEW_CODE = 'DUMMY'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CONDITION_GET_VARIABLE_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LPS_RPC_F  text
*      -->LPS_CHAR  text
*      <--LPS_CONDITION  text
*----------------------------------------------------------------------*
FORM CONDITION_GET_VARIABLE_VALUE
  USING LPS_RPC_F     TYPE ZST0_BM_RPC_F
        LPS_CHAR      TYPE ZST_BM_RPC_DIMFIELD
  CHANGING LPS_CONDITION TYPE ZST_BM_FIELD_COND.

  CASE LPS_CHAR-CHECKFIELD.
    WHEN 'VYEAR'.
      PERFORM CONDITION_GET_VARIABLE_YEAR
        USING LPS_RPC_F-EFMON
        CHANGING LPS_CONDITION-RLOW.

      PERFORM CONDITION_GET_VARIABLE_YEAR
        USING LPS_RPC_F-EFMON
        CHANGING LPS_CONDITION-RHIGH.
    WHEN 'VPOPER'.
      PERFORM CONDITION_GET_VARIABLE_PERIOD
        USING LPS_RPC_F-EFMON
        CHANGING LPS_CONDITION-RLOW.

      PERFORM CONDITION_GET_VARIABLE_PERIOD
        USING LPS_RPC_F-EFMON
        CHANGING LPS_CONDITION-RHIGH.
    WHEN 'VMONAT'.
      PERFORM CONDITION_GET_VARIABLE_MONAT
        USING LPS_RPC_F-EFMON
        CHANGING LPS_CONDITION-RLOW.

      PERFORM CONDITION_GET_VARIABLE_MONAT
        USING LPS_RPC_F-EFMON
        CHANGING LPS_CONDITION-RHIGH.
    WHEN 'VJAHRPER'.
      PERFORM CONDITION_GET_VARIABLE_JAHRPER
        USING LPS_RPC_F-EFMON
        CHANGING LPS_CONDITION-RLOW.

      PERFORM CONDITION_GET_VARIABLE_JAHRPER
        USING LPS_RPC_F-EFMON
        CHANGING LPS_CONDITION-RHIGH.
    WHEN 'VDATE'.
      PERFORM CONDITION_GET_VARIABLE_DATE
        USING LPS_RPC_F-EFMON
        CHANGING LPS_CONDITION-RLOW.

      PERFORM CONDITION_GET_VARIABLE_DATE
        USING LPS_RPC_F-EFMON
        CHANGING LPS_CONDITION-RHIGH.
    WHEN 'VBUKRS'.
      PERFORM CONDITION_GET_VARIABLE_BUKRS
        USING LPS_RPC_F-BUKRS
        CHANGING LPS_CONDITION-RLOW.

      PERFORM CONDITION_GET_VARIABLE_BUKRS
        USING LPS_RPC_F-BUKRS
        CHANGING LPS_CONDITION-RHIGH.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0100_DELETE_ROWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0100_DELETE_ROWS .
  DATA:
    LT_CELLS      TYPE ZTT0_BM_RPC_SICELL.

  CALL FUNCTION 'ZFM_ALV_ROWS_GET_SELECTED'
    EXPORTING
      I_ALV_GRID  = GO_SI_ALV
      IT_ALV_DATA = GS_RPC_DS_SI-CELLS
    IMPORTING
      ET_SEL_DATA = LT_CELLS.
  IF LT_CELLS IS INITIAL.
    MESSAGE S018(ZMS_COL_LIB) DISPLAY LIKE GC_MTYPE_E.
    RETURN.
  ENDIF.

  LOOP AT LT_CELLS INTO DATA(LS_CELL).
    READ TABLE GS_RPC_DS_SI-ROWS INTO DATA(LS_SIR)
      WITH KEY ROWNO = LS_CELL-ROWNO
               HROW  = LS_CELL-HROW.
    IF SY-SUBRC IS INITIAL.
      IF LS_SIR-HROW IS INITIAL.
        DELETE GS_RPC_DS_SI-CONDITIONS WHERE ROWNO = LS_SIR-ROWNO.
        DELETE GS_RPC_DS_SI-ROWS
          WHERE ROWNO = LS_SIR-ROWNO AND HROW  = LS_CELL-HROW.
      ELSE.
        DELETE GS_RPC_DS_SI-ROWS
          WHERE ROWNO = LS_SIR-ROWNO AND HROW  = LS_CELL-HROW.
      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM SI_GEN_DISPLAY_CELLS.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CONDITION_GET_VARIABLE_YEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LPW_EFMON  text
*      <--LPW_VALUE  text
*----------------------------------------------------------------------*
FORM CONDITION_GET_VARIABLE_YEAR
  USING    LPW_EFMON    TYPE ZDD_BM_RPC_EFMON
  CHANGING LPW_VALUE    TYPE CHAR45.

  DATA:
    LW_PYEAR TYPE GJAHR.

  LW_PYEAR = LPW_EFMON(4) - 1.
  REPLACE 'CYR' IN LPW_VALUE WITH LPW_EFMON(4) IN CHARACTER MODE.
  REPLACE 'PYR' IN LPW_VALUE WITH LW_PYEAR IN CHARACTER MODE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CONDITION_GET_VARIABLE_PERIOD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LPW_EFMON  text
*      <--LPW_VALUE  text
*----------------------------------------------------------------------*
FORM CONDITION_GET_VARIABLE_PERIOD
  USING    LPW_EFMON    TYPE ZDD_BM_RPC_EFMON
  CHANGING LPW_VALUE    TYPE CHAR45.
  DATA:
    LW_POPER TYPE POPER,
    LW_CMN   TYPE POPER,
    LW_PMN   TYPE POPER,
    LW_CQB   TYPE POPER,
    LW_CQE   TYPE POPER,
    LW_PQB   TYPE POPER,
    LW_PQE   TYPE POPER.

  LW_POPER = LPW_EFMON+4.
  LW_CMN = LW_POPER.
  LW_PMN = LW_POPER - 1.
  LW_CQB = ( LW_POPER - 1 ) DIV 3 * 3 + 1.
  LW_CQE = ( LW_POPER - 1 ) DIV 3 * 3 + 3.
  IF LW_POPER < 4.
    LW_PQB = LW_CQB.
    LW_PQE = LW_CQE.
  ELSE.
    LW_PQB = ( LW_POPER - 1 ) DIV 3 * 3 - 2.
    LW_PQE = ( LW_POPER - 1 ) DIV 3 * 3.
  ENDIF.

* Current month
  REPLACE 'CMN' IN LPW_VALUE WITH LW_CMN IN CHARACTER MODE.
*	Previous month
  REPLACE 'PMN' IN LPW_VALUE WITH LW_PMN IN CHARACTER MODE.
* Begin month current quarter
  REPLACE 'CQB' IN LPW_VALUE WITH LW_CQB IN CHARACTER MODE.
* End month current quarter
  REPLACE 'CQE' IN LPW_VALUE WITH LW_CQE IN CHARACTER MODE.
*	Begin month previous quarter
  REPLACE 'PQB' IN LPW_VALUE WITH LW_PQB IN CHARACTER MODE.
*	End month previous quarter
  REPLACE 'PQE' IN LPW_VALUE WITH LW_PQE IN CHARACTER MODE.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CONDITION_GET_VARIABLE_MONAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LPW_EFMON  text
*      <--LPW_VALUE  text
*----------------------------------------------------------------------*
FORM CONDITION_GET_VARIABLE_MONAT
  USING    LPW_EFMON    TYPE ZDD_BM_RPC_EFMON
  CHANGING LPW_VALUE    TYPE CHAR45.
  DATA:
    LW_POPER TYPE MONAT,
    LW_CMN   TYPE MONAT,
    LW_PMN   TYPE MONAT,
    LW_CQB   TYPE MONAT,
    LW_CQE   TYPE MONAT,
    LW_PQB   TYPE MONAT,
    LW_PQE   TYPE MONAT.

  LW_POPER = LPW_EFMON+4.
  LW_CMN = LW_POPER.
  LW_PMN = LW_POPER - 1.
  LW_CQB = ( LW_POPER - 1 ) DIV 3 * 3 + 1.
  LW_CQE = ( LW_POPER - 1 ) DIV 3 * 3 + 3.
  IF LW_POPER < 4.
    LW_PQB = LW_CQB.
    LW_PQE = LW_CQE.
  ELSE.
    LW_PQB = ( LW_POPER - 1 ) DIV 3 * 3 - 2.
    LW_PQE = ( LW_POPER - 1 ) DIV 3 * 3.
  ENDIF.

* Current month
  REPLACE 'CMN' IN LPW_VALUE WITH LW_CMN IN CHARACTER MODE.
*	Previous month
  REPLACE 'PMN' IN LPW_VALUE WITH LW_PMN IN CHARACTER MODE.
* Begin month current quarter
  REPLACE 'CQB' IN LPW_VALUE WITH LW_CQB IN CHARACTER MODE.
* End month current quarter
  REPLACE 'CQE' IN LPW_VALUE WITH LW_CQE IN CHARACTER MODE.
*	Begin month previous quarter
  REPLACE 'PQB' IN LPW_VALUE WITH LW_PQB IN CHARACTER MODE.
*	End month previous quarter
  REPLACE 'PQE' IN LPW_VALUE WITH LW_PQE IN CHARACTER MODE.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CONDITION_GET_VARIABLE_JAHRPER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LPW_EFMON  text
*      <--LPW_VALUE  text
*----------------------------------------------------------------------*
FORM CONDITION_GET_VARIABLE_JAHRPER
  USING    LPW_EFMON    TYPE ZDD_BM_RPC_EFMON
  CHANGING LPW_VALUE    TYPE CHAR45.
  DATA:
    LW_POPER   TYPE POPER,
    LW_NUMC3   TYPE NUMC3,
    LW_JAHRPER TYPE ZDD_BM_RPC_VJAHRPER,
    LW_CMN     TYPE ZDD_BM_RPC_VJAHRPER,
    LW_PMN     TYPE ZDD_BM_RPC_VJAHRPER,
    LW_CQB     TYPE ZDD_BM_RPC_VJAHRPER,
    LW_CQE     TYPE ZDD_BM_RPC_VJAHRPER,
    LW_PQB     TYPE ZDD_BM_RPC_VJAHRPER,
    LW_PQE     TYPE ZDD_BM_RPC_VJAHRPER,
    LW_PYM     TYPE ZDD_BM_RPC_VJAHRPER,
    LW_PYQB    TYPE ZDD_BM_RPC_VJAHRPER,
    LW_PYQE    TYPE ZDD_BM_RPC_VJAHRPER.

  LW_POPER      = LPW_EFMON+4.
  LW_JAHRPER(4) = LPW_EFMON(4).
  LW_JAHRPER+4  = LW_POPER.
  LW_CMN = LW_PMN = LW_CQB = LW_CQE = LW_PQB = LW_PQE = LW_PYM = LW_JAHRPER.
  LW_PYM(4) = LW_PYM(4) - 1.

  LW_CMN+4 = LW_POPER.
  LW_NUMC3 = LW_POPER - 1.
  LW_PMN+4 = LW_NUMC3.
  LW_NUMC3 = ( LW_POPER - 1 ) DIV 3 * 3 + 1.
  LW_CQB+4 = LW_NUMC3.
  LW_NUMC3 = ( LW_POPER - 1 ) DIV 3 * 3 + 3.
  LW_CQE+4 = LW_NUMC3.
  IF LW_POPER < 4.
    LW_PQB = LW_CQB.
    LW_PQE = LW_CQE.
  ELSE.
    LW_NUMC3 = ( LW_POPER - 1 ) DIV 3 * 3 - 2.
    LW_PQB+4 = LW_NUMC3.
    LW_NUMC3 = ( LW_POPER - 1 ) DIV 3 * 3.
    LW_PQE+4 = LW_NUMC3.
  ENDIF.
  LW_PYQB     = LW_CQB.
  LW_PYQB(4)  = LW_PYQB(4) - 1.
  LW_PYQE     = LW_CQE.
  LW_PYQE(4)  = LW_PYQE(4) - 1.

* Current month
  REPLACE 'CMN' IN LPW_VALUE WITH LW_CMN IN CHARACTER MODE.
*	Previous month
  REPLACE 'PMN' IN LPW_VALUE WITH LW_PMN IN CHARACTER MODE.
* Begin month current quarter
  REPLACE 'CQB' IN LPW_VALUE WITH LW_CQB IN CHARACTER MODE.
* End month current quarter
  REPLACE 'CQE' IN LPW_VALUE WITH LW_CQE IN CHARACTER MODE.
*	Begin month previous quarter
  REPLACE 'PQB' IN LPW_VALUE WITH LW_PQB IN CHARACTER MODE.
*	End month previous quarter
  REPLACE 'PQE' IN LPW_VALUE WITH LW_PQE IN CHARACTER MODE.
*	Same month previous year
  REPLACE 'PYM' IN LPW_VALUE WITH LW_PYM IN CHARACTER MODE.
*	Begin quarter  previous year
  REPLACE 'PYQB' IN LPW_VALUE WITH LW_PYQB IN CHARACTER MODE.
*	End quarter  previous year
  REPLACE 'PYQE' IN LPW_VALUE WITH LW_PYQE IN CHARACTER MODE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CONDITION_GET_VARIABLE_DATE
*&---------------------------------------------------------------------*
*       text
*&---------------------------------------------------------------------*
*      -->LPW_EFMON  text
*      <--LPW_VALUE  text
*&---------------------------------------------------------------------*
FORM CONDITION_GET_VARIABLE_DATE
  USING    LPW_EFMON    TYPE ZDD_BM_RPC_EFMON
  CHANGING LPW_VALUE    TYPE CHAR45.

  DATA:
    LW_PYEAR TYPE GJAHR,
    LW_DATE  TYPE DATUM.

  LW_PYEAR = LPW_EFMON(4) - 1.

  CASE LPW_VALUE.
    WHEN 'YYYYMM01'.
      REPLACE 'YYYYMM' IN LPW_VALUE WITH LPW_EFMON(6) IN CHARACTER MODE.
    WHEN 'YYYYMM31'.
      LW_DATE = LPW_EFMON && '01'.
      CALL FUNCTION 'ZFM_GET_FIRST_END_DATE_PERIOD'
        EXPORTING
          I_DATE      = LW_DATE
          I_FOR_MONTH = 'X'
        IMPORTING
          E_ENDDA     = LW_DATE.
      LPW_VALUE = LW_DATE.
    WHEN OTHERS.
      REPLACE 'YYYY' IN LPW_VALUE WITH LPW_EFMON(4) IN CHARACTER MODE.
      REPLACE 'PYYY' IN LPW_VALUE WITH LW_PYEAR IN CHARACTER MODE.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CONDITION_GET_VARIABLE_BUKRS
*&---------------------------------------------------------------------*
*       text
*&---------------------------------------------------------------------*
*      -->LPW_EFMON  text
*      <--LPW_VALUE  text
*&---------------------------------------------------------------------*
FORM CONDITION_GET_VARIABLE_BUKRS
  USING    LPW_BUKRS    TYPE BUKRS
  CHANGING LPW_VALUE    TYPE CHAR45.

  CASE LPW_VALUE.
    WHEN '&BUK'.
      REPLACE '&BUK' IN LPW_VALUE WITH LPW_BUKRS IN CHARACTER MODE.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0320_UPDATE_TEXT_TO_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0320_UPDATE_TEXT_TO_ALV .
  DATA:
    LS_TDTEXT TYPE EFG_STRN_TDTEXT,
    LT_STREAM TYPE TABLE OF TEXT132,
    LW_INDEX  TYPE I.

  CALL METHOD GO_F_TEXT->GET_TEXT_AS_STREAM
    IMPORTING
      TEXT                   = LT_STREAM
    EXCEPTIONS
      ERROR_DP               = 1
      ERROR_CNTL_CALL_METHOD = 2
      OTHERS                 = 3.

  LS_TDTEXT-TDNAME = ZVI_BM_RPC_F-REPID && ZVI_BM_RPC_F-EFMON
                  && ZVI_BM_RPC_F-RPSEG  && ZVI_BM_RPC_F-RITEM
                  && ZST0_BM_RPC_DIMSET-ROWNO.
  LS_TDTEXT-TDSPRAS = ZST0_BM_RPC_DIMSET-SPRAS.

  READ TABLE GS_RPC_IN_F-TEXTS ASSIGNING FIELD-SYMBOL(<LF_TDTEXT>)
    WITH KEY TDNAME   = LS_TDTEXT-TDNAME
             TDSPRAS  = LS_TDTEXT-TDSPRAS.
  CHECK SY-SUBRC IS INITIAL.

  READ TABLE GS_RPC_IN_F-DATA ASSIGNING FIELD-SYMBOL(<LF_DATA>)
    WITH KEY ROWPOS = ZST0_BM_RPC_DIMSET-ROWNO.
  CHECK SY-SUBRC IS INITIAL.

  DATA:
    LS_DYNPREAD TYPE DYNPREAD,
    LT_DYNPREAD TYPE TABLE OF DYNPREAD.

  LS_DYNPREAD-FIELDNAME = 'ZST0_BM_RPC_DIMSET-CSTYLE'.
  APPEND LS_DYNPREAD TO LT_DYNPREAD.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      DYNAME     = SY-REPID
      DYNUMB     = SY-DYNNR
    TABLES
      DYNPFIELDS = LT_DYNPREAD
    EXCEPTIONS
      OTHERS     = 11.
  READ TABLE LT_DYNPREAD INTO LS_DYNPREAD INDEX 1.
  ZST0_BM_RPC_DIMSET-CSTYLE = LS_DYNPREAD-FIELDVALUE.

  DO GC_MAXCOL TIMES.
    LW_INDEX = SY-INDEX - 1.
    <LF_DATA>-LSTYLE+LW_INDEX(1) = ZST0_BM_RPC_DIMSET-CSTYLE.
  ENDDO.

  CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
    EXPORTING
*     STREAM_LINES = LT_STREAM
      LANGUAGE    = ZST0_BM_RPC_DIMSET-SPRAS
    TABLES
      TEXT_STREAM = LT_STREAM
      ITF_TEXT    = <LF_TDTEXT>-T_LINES.

  READ TABLE <LF_TDTEXT>-T_LINES INTO DATA(LS_LINE) INDEX 1.

  IF ZST0_BM_RPC_DIMSET-SPRAS = 'E'.
    <LF_DATA>-TEX01 = LS_LINE-TDLINE.
  ELSE.
    <LF_DATA>-TEX02 = LS_LINE-TDLINE.
  ENDIF.

  PERFORM FORM_GEN_DISPLAY_CELLS_TEXT.

  CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
    EXPORTING
      I_ALV_GRID = GO_F_ALV.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0000_INIT_PROC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0000_INIT_PROC .
  SELECT DISTINCT TABNAME
    FROM ZTB_BM_RPC_DIM
    INTO TABLE GT_SRCTAB
   WHERE TABNAME NE SPACE.
  SORT GT_SRCTAB BY TABLE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CONDITION_CONVERSION_EXIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--LPS_SRC_COND  text
*----------------------------------------------------------------------*
FORM CONDITION_CONVERSION_EXIT
  CHANGING LPS_SICOND TYPE ZST0_BM_RPC_SICOND.
  DATA:
    LR_DATA     TYPE REF TO DATA,
    LW_FUNCNAME TYPE CHAR30.
  FIELD-SYMBOLS:
    <LF_VALUE>  TYPE ANY.

  IF LPS_SICOND-RSIGN IS INITIAL.
    LPS_SICOND-RSIGN = 'I'.
  ENDIF.
  IF LPS_SICOND-ROPTI IS INITIAL.
    IF LPS_SICOND-RHIGH IS INITIAL.
      LPS_SICOND-ROPTI = 'EQ'.
    ELSE.
      LPS_SICOND-ROPTI = 'BT'.
    ENDIF.
  ENDIF.

  READ TABLE GT_RPC_CHAR INTO DATA(LS_CHAR)
    WITH KEY TABNAME = LPS_SICOND-RTABLE
             FIELDNAME = LPS_SICOND-RFIELD BINARY SEARCH.
  IF SY-SUBRC IS INITIAL
  AND LS_CHAR-CONVEXIT IS NOT INITIAL
  AND LS_CHAR-CHECKTABLE <> 'ZST_BM_RPC_VAR'.
    CREATE DATA LR_DATA TYPE (LS_CHAR-ROLLNAME).
    ASSIGN LR_DATA->* TO <LF_VALUE>.
    LW_FUNCNAME = 'CONVERSION_EXIT_' && LS_CHAR-CONVEXIT && '_INPUT'.

    <LF_VALUE> = LPS_SICOND-RLOW.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT':
    CALL FUNCTION LW_FUNCNAME
      EXPORTING
        INPUT  = <LF_VALUE>
      IMPORTING
        OUTPUT = <LF_VALUE>.
    LPS_SICOND-RLOW = <LF_VALUE>.

    <LF_VALUE> = LPS_SICOND-RHIGH.
    CALL FUNCTION LW_FUNCNAME
      EXPORTING
        INPUT  = <LF_VALUE>
      IMPORTING
        OUTPUT = <LF_VALUE>.
    LPS_SICOND-RHIGH = <LF_VALUE>.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0200_FC_LSTYLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0200_FC_LSTYLE .
  DATA:
    LW_INDEX  TYPE I,
    LW_LSTYLE TYPE ZDD_BM_RPC_LSTYLE.

  DO GC_MAXCOL TIMES.
    LW_INDEX = SY-INDEX - 1.
    LW_LSTYLE+LW_INDEX(1) = ZST0_BM_RPC_DIMSET-CSTYLE.
  ENDDO.
  GS_LSTYLE = LW_LSTYLE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FORM_COVERT_DATA_TO_LEADDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LPS_RPC_F  text
*      <--LPS_TEMPL_DATA  text
*----------------------------------------------------------------------*
FORM FORM_COVERT_DATA_TO_LEADDATA
  USING    LPS_RPC_F      TYPE ZST0_BM_RPC_F
  CHANGING LPS_TEMPL_DATA TYPE ZST1_BM_RPC_SI_TEMPL.
  DATA:
    LW_FIELDNAME  TYPE FIELDNAME,
    LW_LEADCOL    TYPE ZDD_BM_RPC_COLNO,
    LT_LINEOUTPUT TYPE TABLE OF ZST0_BM_RPC_LINEOUTPUT,
    LS_LEADDATA   TYPE ZST1_BM_RPC_LD,
    LS_DATA       TYPE ZST0_BM_RPC_D.

  MOVE-CORRESPONDING LPS_TEMPL_DATA-DATA TO LT_LINEOUTPUT.
  LW_LEADCOL = LPS_RPC_F-SEGITM-NOLEAD - 1.
  LW_FIELDNAME = 'CEL' && LW_LEADCOL.

  LOOP AT LT_LINEOUTPUT INTO DATA(LS_LINE_OUTPUT).
    AT NEW (LW_FIELDNAME).
      CLEAR: LS_LEADDATA.
      MOVE-CORRESPONDING LS_LINE_OUTPUT TO LS_LEADDATA.
    ENDAT.

    CLEAR: LS_DATA.
    MOVE-CORRESPONDING LS_LINE_OUTPUT TO LS_DATA.
    APPEND LS_DATA TO LS_LEADDATA-DATA.

    AT END OF (LW_FIELDNAME).
      APPEND LS_LEADDATA TO LPS_TEMPL_DATA-LEADDATA.
    ENDAT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  0210_FC_SRCTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM 0210_FC_SRCTAB .
  IF ZST0_BM_RPC_DIMSET-ROWNO IS INITIAL.
    DELETE GS_RPC_DS_SI-CONDITIONS
      WHERE COLNO = ZST0_BM_RPC_DIMSET-COLNO.
  ELSE.
    DELETE GS_RPC_DS_SI-CONDITIONS
      WHERE ROWNO = ZST0_BM_RPC_DIMSET-ROWNO.
  ENDIF.
  PERFORM GEN_CONDITION_ALV.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0100_BACK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0100_SAVE .
  DATA:
    LS_SI_NEW TYPE GTY_SI_DB,
    LS_SI_ORD TYPE GTY_SI_DB.

  PERFORM CONVERT_SI_TO_DB
    USING GS_RPC_DS_SI
    CHANGING LS_SI_NEW.

  PERFORM CONVERT_SI_TO_DB
    USING GS_RPC_DS_SI_O
    CHANGING LS_SI_ORD.

  IF LS_SI_NEW = LS_SI_ORD.
    MESSAGE S360(SV) DISPLAY LIKE GC_MTYPE_W.
    RETURN.
  ENDIF.

  PERFORM SEGMENT_ITEM_SAVE
    USING LS_SI_NEW
          LS_SI_ORD.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0100_BACK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0100_BACK .
  DATA:
    LW_ANSWER TYPE C,
    LS_SI_NEW TYPE GTY_SI_DB,
    LS_SI_ORD TYPE GTY_SI_DB.

  PERFORM CONVERT_SI_TO_DB
    USING GS_RPC_DS_SI
    CHANGING LS_SI_NEW.

  PERFORM CONVERT_SI_TO_DB
    USING GS_RPC_DS_SI_O
    CHANGING LS_SI_ORD.

  IF LS_SI_NEW <> LS_SI_ORD.
*  IF GS_RPC_DS_SI <> GS_RPC_DS_SI_O.
    CALL FUNCTION 'ZFM_POPUP_CONFIRM_SAVE_CHANGED'
      EXPORTING
        I_DISPLAY_CANCEL_BUTTON = ''
      IMPORTING
        E_ANSWER                = LW_ANSWER.
    IF LW_ANSWER = '1'.
      PERFORM SEGMENT_ITEM_SAVE
        USING LS_SI_NEW
              LS_SI_ORD.
    ENDIF.
  ENDIF.

  CALL METHOD GO_SI_ALV->FREE.
  CALL METHOD GO_SI_CON->FREE.
  FREE: GO_SI_ALV, GO_SI_CON.
  LEAVE TO SCREEN 0.
  CALL METHOD CL_GUI_CFW=>FLUSH.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0100_EXIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0100_EXIT .
  DATA:
    LW_ANSWER TYPE C,
    LS_SI_NEW TYPE GTY_SI_DB,
    LS_SI_ORD TYPE GTY_SI_DB.

  PERFORM CONVERT_SI_TO_DB
    USING GS_RPC_DS_SI
    CHANGING LS_SI_NEW.

  PERFORM CONVERT_SI_TO_DB
    USING GS_RPC_DS_SI_O
    CHANGING LS_SI_ORD.

  IF LS_SI_NEW <> LS_SI_ORD.
*  IF GS_RPC_DS_SI <> GS_RPC_DS_SI_O.
    CALL FUNCTION 'ZFM_POPUP_CONFIRM_SAVE_CHANGED'
      EXPORTING
        I_DISPLAY_CANCEL_BUTTON = ''
      IMPORTING
        E_ANSWER                = LW_ANSWER.
    IF LW_ANSWER = '1'.
      PERFORM SEGMENT_ITEM_SAVE
        USING LS_SI_NEW
              LS_SI_ORD.
    ENDIF.
  ENDIF.

  LEAVE PROGRAM.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0210_ADDCOND
*&---------------------------------------------------------------------*
*       Add condition
*----------------------------------------------------------------------*
FORM 0210_ADDCOND .
  DATA:
    LT_SELOPT     TYPE TABLE OF ZST0_BM_RPC_SELOPT,
    LS_SELOPT     TYPE ZST0_BM_RPC_SELOPT,
    LS_SICOND     TYPE ZST0_BM_RPC_SICOND,
    LW_RETURNCODE TYPE C.

  CALL FUNCTION 'ZFM_ALV_ROWS_GET_SELECTED'
    EXPORTING
      I_ALV_GRID  = GO_SI_COND
      IT_ALV_DATA = GT_CONDITION
    IMPORTING
      ET_SEL_DATA = LT_SELOPT.

  IF LT_SELOPT IS INITIAL.
    MESSAGE S018(ZMS_COL_LIB) DISPLAY LIKE GC_MTYPE_W.
    RETURN.
  ENDIF.

  CALL FUNCTION 'ZFM_POPUP_SET_DATA_RECORD'
    EXPORTING
      I_POPUP_TITLE = TEXT-001
      I_SUB_TABNAME = 'ZST0_BM_RPC_SELOPT'
      I_SUB_FNAME   = 'CONDID'
    IMPORTING
      RETURNCODE    = LW_RETURNCODE
    CHANGING
      C_RECORD      = LS_SELOPT.
  CHECK LW_RETURNCODE IS INITIAL.

  MODIFY LT_SELOPT FROM LS_SELOPT
    TRANSPORTING CONDID WHERE CONDID <> LS_SELOPT-CONDID.
  APPEND LINES OF LT_SELOPT TO GT_CONDITION.
  LOOP AT LT_SELOPT INTO LS_SELOPT.
    CLEAR: LS_SICOND.
    LS_SICOND-COLNO   = ZST0_BM_RPC_DIMSET-COLNO.
    LS_SICOND-ROWNO   = ZST0_BM_RPC_DIMSET-ROWNO.
    LS_SICOND-RTABLE  = ZST0_BM_RPC_DIMSET-SRCTAB.
    LS_SICOND-RFIELD  = LS_SELOPT-FIELDNAME.
    LS_SICOND-CONDID  = LS_SELOPT-CONDID.
    LS_SICOND-RANGID  = 1.
    APPEND LS_SICOND TO GS_RPC_DS_SI-CONDITIONS.
  ENDLOOP.
  PERFORM GEN_CONDITION_ALV.
  CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
    EXPORTING
      I_ALV_GRID = GO_SI_COND.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0100_ADD_HIDDEN_COLS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0100_ADD_HIDDEN_COLS .
  DATA:
    LS_COL TYPE ZST0_BM_RPC_SIC.

* Generate columns
  READ TABLE GS_RPC_DS_SI-COLUMNS INTO LS_COL
    INDEX LINES( GS_RPC_DS_SI-COLUMNS ).
  IF SY-SUBRC IS INITIAL.
    LS_COL-COLNO = LS_COL-COLNO + 1.
    IF LS_COL-COLNO > GC_MAXCOL.
      MESSAGE S030(ZMS_COL_LIB) DISPLAY LIKE GC_MTYPE_E.
      RETURN.
    ENDIF.
    LS_COL-FIELDNAME = 'COL' && LS_COL-COLNO.
    LS_COL-SRCTAB = 'ACDOCA'.

    READ TABLE GS_RPC_DS_SI-FCAT ASSIGNING FIELD-SYMBOL(<LF_FCAT>)
      WITH KEY FIELDNAME = LS_COL-FIELDNAME.
    IF SY-SUBRC IS INITIAL.
      <LF_FCAT>-NO_OUT = SPACE.
    ENDIF.
    APPEND LS_COL TO GS_RPC_DS_SI-COLUMNS.
  ENDIF.

  PERFORM PREPARE_OUTPUT_SI_TABLE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0100_DELETE_COLS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0100_DELETE_COLS .
  DATA:
    LS_COL TYPE ZST0_BM_RPC_SIC.

  DELETE GS_RPC_DS_SI-COLUMNS
    WHERE COLNO > GS_RPC_DS_SI-NOCOL.

  PERFORM PREPARE_OUTPUT_SI_TABLE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CELL_CONVERT_VARIABLE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM CELL_CONVERT_VARIABLE
  USING    LPS_RPC_F    TYPE ZST0_BM_RPC_F
  CHANGING LPW_VALUE    .


  DATA:
    LW_PYEAR TYPE GJAHR,
    LW_PRMON TYPE CHAR2.

  CHECK LPW_VALUE CS '&'.
  LW_PYEAR = LPS_RPC_F-EFMON(4) - 1.
  LW_PRMON = LPS_RPC_F-EFMON+4(2) - 1.

  REPLACE '&BUK&' IN LPW_VALUE WITH LPS_RPC_F-BUKRS IN CHARACTER MODE.
  REPLACE '&BUT&' IN LPW_VALUE WITH LPS_RPC_F-BUTXT IN CHARACTER MODE.

  REPLACE '&BMN&' IN LPW_VALUE WITH LPS_RPC_F-BEMON+4(2) IN CHARACTER MODE.
  REPLACE '&EMN&' IN LPW_VALUE WITH LPS_RPC_F-ENMON+4(2) IN CHARACTER MODE.

  REPLACE '&CMN&' IN LPW_VALUE WITH LPS_RPC_F-EFMON+4(2) IN CHARACTER MODE.
  REPLACE '&PMN&' IN LPW_VALUE WITH LW_PRMON IN CHARACTER MODE.

  REPLACE '&CYR&' IN LPW_VALUE WITH LPS_RPC_F-EFMON(4) IN CHARACTER MODE.
  REPLACE '&PYR&' IN LPW_VALUE WITH LW_PYEAR IN CHARACTER MODE.

  REPLACE '&CYR2&' IN LPW_VALUE WITH LPS_RPC_F-EFMON+2(2) IN CHARACTER MODE.
  REPLACE '&PYR2&' IN LPW_VALUE WITH LW_PYEAR+2(2) IN CHARACTER MODE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DATA .
  SELECT *
    FROM ZTB_BM_RPC_W
    INTO TABLE GT_BM_RPC_W
  WHERE REPID = GW_REPORTID
    .

  SELECT *
    FROM ZTB_BM_RPC_WC
    INTO TABLE GT_BM_RPC_WC
  WHERE REPID = GW_REPORTID
    AND BUKRS = GW_BUKRS
    .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROC_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM PROC_DATA.
  DATA: LS_BM_RPC_WC TYPE ZTB_BM_RPC_WC,
        LS_HEADER    TYPE THEAD,
        LW_NAME      TYPE TDOBNAME,
        LW_NF        TYPE MARK.

  FIELD-SYMBOLS:
    <LF_RP_WORDING> TYPE ANY,
    <LF_VALUE>      TYPE ANY.

  ASSIGN GS_STRUC->* TO <GF_RP_WORDING>.

  LOOP AT GT_BM_RPC_W INTO DATA(LS_BM_RPC_W).
    CLEAR: LW_NAME, LS_HEADER, LW_NF.
    READ TABLE GT_BM_RPC_WC INTO LS_BM_RPC_WC
    WITH KEY RITEM = LS_BM_RPC_W-RITEM
    .
    IF SY-SUBRC IS INITIAL.
      CONCATENATE LS_BM_RPC_WC-REPID LS_BM_RPC_WC-RITEM LS_BM_RPC_WC-BUKRS INTO LW_NAME.

      LS_HEADER-TDOBJECT   = 'ZRPC'.
      LS_HEADER-TDNAME     = LW_NAME.
      LS_HEADER-TDID       = 'Z002'.
      LS_HEADER-TDSPRAS    = 'E'.
      LS_HEADER-TDLINESIZE = 70.

      PERFORM READ_TEXT USING LS_HEADER
                        CHANGING LW_NF. "Not found

      IF LW_NF IS NOT INITIAL.
        CLEAR: LW_NAME.
        CONCATENATE LS_BM_RPC_W-REPID LS_BM_RPC_W-RITEM INTO LW_NAME.

        ASSIGN COMPONENT LS_BM_RPC_W-RITEM OF STRUCTURE <GF_RP_WORDING>
        TO <LF_VALUE>.
        .
        IF SY-SUBRC IS INITIAL.
          <LF_VALUE> = LW_NAME.
        ENDIF.
      ELSE.
        CLEAR: LW_NAME.
        CONCATENATE LS_BM_RPC_WC-REPID LS_BM_RPC_WC-RITEM LS_BM_RPC_WC-BUKRS INTO LW_NAME.
        ASSIGN COMPONENT LS_BM_RPC_WC-RITEM OF STRUCTURE <GF_RP_WORDING>
        TO <LF_VALUE>.
        .
        IF SY-SUBRC IS INITIAL.
          <LF_VALUE> = LW_NAME.
        ENDIF.
*        LS_HEADER-TDNAME     = LW_NAME.
*        PERFORM READ_TEXT USING LS_HEADER
*                          CHANGING LW_NF. "Not found

      ENDIF.
    ELSE.
      "Get default
      CLEAR: LW_NAME.
      CONCATENATE LS_BM_RPC_W-REPID LS_BM_RPC_W-RITEM INTO LW_NAME.
      ASSIGN COMPONENT LS_BM_RPC_W-RITEM OF STRUCTURE <GF_RP_WORDING>
        TO <LF_VALUE>.
      .
      IF SY-SUBRC IS INITIAL.
        <LF_VALUE> = LW_NAME.
      ENDIF.


*      LS_HEADER-TDOBJECT   = 'ZRPC'.
*      LS_HEADER-TDNAME     = LW_NAME.
*      LS_HEADER-TDID       = 'Z002'.
*      LS_HEADER-TDSPRAS    = 'E'.
*      LS_HEADER-TDLINESIZE = 70.
*
*      PERFORM READ_TEXT USING LS_HEADER
*                        CHANGING LW_NF. "Not found
    ENDIF.
  ENDLOOP.

*  GS_RP_WORDING = <GF_RP_WORDING>.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form READ_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_HEADER
*&---------------------------------------------------------------------*
FORM READ_TEXT  USING    LPS_HEADER TYPE THEAD
                CHANGING LPW_NF     TYPE MARK.
  DATA:
        LW_SUCC  TYPE MARK,

        LT_LINES TYPE TABLE OF TLINE.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      ID                      = LPS_HEADER-TDID
      LANGUAGE                = LPS_HEADER-TDSPRAS
      NAME                    = LPS_HEADER-TDNAME
      OBJECT                  = LPS_HEADER-TDOBJECT
    TABLES
      LINES                   = LT_LINES
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.
  IF SY-SUBRC <> 0.
    LPW_NF = GC_MARK.
  ELSE.
    IF LT_LINES IS INITIAL.
      LPW_NF = GC_MARK.
    ELSE.
      LOOP AT LT_LINES INTO DATA(LS_LINES).
        IF LS_LINES-TDLINE IS NOT INITIAL.
          LW_SUCC = GC_MARK.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF LW_SUCC IS INITIAL.
        LPW_NF = GC_MARK.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_TABLE_WITH_KEYS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE_TABLE_WITH_KEYS USING LPW_STRUC TYPE TABNAME.

  CREATE DATA GS_STRUC TYPE (LPW_STRUC).
ENDFORM.
