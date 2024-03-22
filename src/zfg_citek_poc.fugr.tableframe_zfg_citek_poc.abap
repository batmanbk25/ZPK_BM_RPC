*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZFG_CITEK_POC
*   generation date: 22.07.2019 at 15:31:27
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZFG_CITEK_POC      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
