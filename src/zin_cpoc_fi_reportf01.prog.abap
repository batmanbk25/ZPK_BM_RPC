*&---------------------------------------------------------------------*
*&  Include           ZIN_CPOC_FI_REPORTF01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  0000_INIT_PROC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0000_INIT_PROC .
  P_REPID = 'ZCO01'.
  GET PARAMETER ID 'BUK' FIELD P_BUKRS.
  SELECT SINGLE REPNM
    INTO P_REPNM
    FROM ZTB_BM_RPC_R
   WHERE REPID = P_REPID.

  SELECT SINGLE BUTXT
    INTO P_BUTXT
    FROM T001
   WHERE BUKRS = P_BUKRS.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  0000_MAIN_PROC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 0000_MAIN_PROC .
  DATA:
    LT_RPC_D TYPE TABLE OF ZTB_BM_RPC_D,
    LS_RPC_R TYPE ZST1_BM_RPC_R,
    LW_LANGU TYPE LANGU VALUE 'E'.

  IF P_VI IS NOT INITIAL.
    LW_LANGU = '쁩'.
  ENDIF.

*  BREAK-POINT.
  CALL FUNCTION 'ZFM_BM_RPC_GENERATE'
    EXPORTING
      I_REPID        = P_REPID
      I_BUKRS        = P_BUKRS
      I_BUTXT        = P_BUTXT
      I_BEMON        = S_EFMON-LOW
      I_ENMON        = S_EFMON-HIGH
*      I_MONTH        = P_EFMON
      I_LANGU        = LW_LANGU
    CHANGING
      CS_RPC_CONTEXT = LS_RPC_R.

  IF LS_RPC_R-XFORM IS INITIAL.
    LS_RPC_R-XFORM = 'ZXLSX_RPC_MBAL'.
  ENDIF.

  CALL FUNCTION 'ZFM_BM_XLWB_CALLFORM'
    EXPORTING
      IV_FORMNAME    = LS_RPC_R-XFORM
      IV_CONTEXT_REF = LS_RPC_R
*     IV_VIEWER_TITLE   = SY-TITLE
*     IV_VIEWER_INPLACE = SPACE
    .

ENDFORM.
*  RETURN.
*
*  SELECT *
*    FROM ZTB_BM_RPC_D
*    INTO TABLE LT_RPC_D
*   WHERE REPID = P_REPID
*     AND EFMON = P_EFMON.
*  SORT LT_RPC_D BY REPID EFMON RPSEG RITEM ROWPOS.
*
*
*  GS_RPC_POC-EN     = P_EN.
*  GS_RPC_POC-VI     = P_VI.
*  IF P_EN IS INITIAL.
*    DELETE LT_RPC_D WHERE LANGU = 'E'.
*  ELSE.
*    DELETE LT_RPC_D WHERE LANGU = '쁩'.
*  ENDIF.
*
*  GS_RPC_POC-TAB5180 = GS_RPC_POC-TAB5190 = GS_RPC_POC-TAB5200
*                     = GS_RPC_POC-TAB5300 = GS_RPC_POC-TAB5400
*                     = GS_RPC_POC-TAB5100 = GS_RPC_POC-TAB5390
*                     = LT_RPC_D.
*  DELETE GS_RPC_POC-TAB5100 WHERE RPSEG <> '5' OR RITEM <> '100'.
*  DELETE GS_RPC_POC-TAB5180 WHERE RPSEG <> '5' OR RITEM <> '180'.
*  DELETE GS_RPC_POC-TAB5180 WHERE RPSEG <> '5' OR RITEM <> '180'.
*  DELETE GS_RPC_POC-TAB5190 WHERE RPSEG <> '5' OR RITEM <> '190'.
*  DELETE GS_RPC_POC-TAB5200 WHERE RPSEG <> '5' OR RITEM <> '200'.
*  DELETE GS_RPC_POC-TAB5300 WHERE RPSEG <> '5' OR RITEM <> '300'.
*  DELETE GS_RPC_POC-TAB5390 WHERE RPSEG <> '5' OR RITEM <> '390'.
*  DELETE GS_RPC_POC-TAB5400 WHERE RPSEG <> '5' OR RITEM <> '400'.
*
*  GS_RPC_POC-REPID = P_REPID.
*  GS_RPC_POC-REPNM = P_REPNM.
*
*  CALL FUNCTION 'ZXLWB_CALLFORM'
*    EXPORTING
*      IV_FORMNAME       = 'ZXLSX_RPC_MB001'
*      IV_CONTEXT_REF    = GS_RPC_POC
**     IV_VIEWER_TITLE   = SY-TITLE
*      IV_VIEWER_INPLACE = SPACE "'X'
**     IV_VIEWER_CALLBACK_PROG       = SY-CPROG
**     IV_VIEWER_CALLBACK_FORM       =
**     IV_VIEWER_SUPPRESS            =
**     IV_PROTECT        =
**     IV_SAVE_AS        =
**     IV_STARTUP_MACRO  =
**     IT_DOCPROPERTIES  =
**   IMPORTING
**     EV_DOCUMENT_RAWDATA           =
**   EXCEPTIONS
**     PROCESS_TERMINATED            = 1
**     OTHERS            = 2
*    .
*  IF SY-SUBRC <> 0.
** Implement suitable error handling here
*  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  1000_PBO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 1000_PBO .

  LOOP AT SCREEN.
    CASE SCREEN-NAME.
      WHEN 'P_REPNM' OR 'P_BUTXT'.
        SCREEN-INPUT = '0'.
        SCREEN-DISPLAY_3D = '0'.
        MODIFY SCREEN.
      WHEN 'P_REPNM'.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  1000_PAI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM 1000_PAI .
  SELECT SINGLE REPNM
    INTO P_REPNM
    FROM ZTB_BM_RPC_R
   WHERE REPID = P_REPID.

  SELECT SINGLE BUTXT
    INTO P_BUTXT
    FROM T001
   WHERE BUKRS = P_BUKRS.
ENDFORM.
