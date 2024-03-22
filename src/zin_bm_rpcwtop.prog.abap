*&---------------------------------------------------------------------*
*& Include ZIN_BM_RPCWTOP                           - Report ZPG_BM_RPCW
*&---------------------------------------------------------------------*
REPORT ZPG_BM_RPCW.
TABLES: ZTB_BM_RPC_W.
DATA:
    GW_BUKRS  TYPE BUKRS.

SELECTION-SCREEN BEGIN OF BLOCK PAR WITH FRAME TITLE TEXT-001.
  PARAMETERS:
    P_REPID TYPE ZTB_BM_RPC_W-REPID OBLIGATORY MATCHCODE OBJECT ZSH_REPID,
    P_BUKRS TYPE BUKRS OBLIGATORY.

*  SELECT-OPTIONS:
*    S_BUKRS FOR GW_BUKRS.
SELECTION-SCREEN END OF BLOCK PAR.
