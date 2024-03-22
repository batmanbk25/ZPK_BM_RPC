FUNCTION ZFM_BM_RPC_WORDING_GET.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_BUKRS) TYPE  BUKRS
*"     REFERENCE(I_REPORTID) TYPE  ZDD_BM_RPC_REPID
*"     REFERENCE(I_STRUCTURE) TYPE  TABNAME
*"  EXPORTING
*"     REFERENCE(ES_RP_WORDING) TYPE  ANY
*"----------------------------------------------------------------------
  CLEAR: GT_BM_RPC_W, GT_BM_RPC_WC.

  GW_BUKRS    = I_BUKRS.
  GW_REPORTID = I_REPORTID.

  PERFORM CREATE_TABLE_WITH_KEYS USING I_STRUCTURE.

  PERFORM GET_DATA.

  PERFORM PROC_DATA.

  ES_RP_WORDING = <GF_RP_WORDING>.

ENDFUNCTION.
