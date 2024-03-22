*&---------------------------------------------------------------------*
*& Include ZIN_CPOC_FI_REPORTTOP          Report ZPG_CPOC_FI_REPORT
*&
*&---------------------------------------------------------------------*

**********************************************************************
* DATA
**********************************************************************
TABLES:
  ZTB_BM_RPC_F.
DATA:
  GS_RPC_POC TYPE ZST_CITEK_POC.

**********************************************************************
* SELECT_OPTIONS AND PARAMETERS
**********************************************************************
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (33) TEXT-001 FOR FIELD P_REPID VISIBLE LENGTH 32.
  PARAMETERS:
    P_REPID TYPE ZST1_BM_RPC_R-REPID DEFAULT 'MB001'.
  SELECTION-SCREEN POSITION 48.
  PARAMETERS
    P_REPNM TYPE ZST1_BM_RPC_R-REPNM.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (33) FOR FIELD P_BUKRS VISIBLE LENGTH 32.
  PARAMETERS:
    P_BUKRS TYPE ZST1_BM_RPC_R-BUKRS MEMORY ID BUK.
  SELECTION-SCREEN POSITION 42.
  PARAMETERS
    P_BUTXT TYPE ZST1_BM_RPC_R-BUTXT.
SELECTION-SCREEN END OF LINE.

SELECT-OPTIONS:
  S_EFMON  FOR ZTB_BM_RPC_F-EFMON NO-EXTENSION DEFAULT SY-DATUM.
PARAMETERS:
  P_EFMON TYPE ZTB_BM_RPC_F-EFMON DEFAULT SY-DATUM NO-DISPLAY,
  P_EN    TYPE ZST1_BM_RPC_R-EN RADIOBUTTON GROUP LANG DEFAULT 'X',
  P_VI    TYPE ZST1_BM_RPC_R-VI RADIOBUTTON GROUP LANG.
