*&---------------------------------------------------------------------*
*& Include          ZIN_BM_RPCWF01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form MAIN_PROC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM MAIN_PROC .
  DATA:
    LS_SELLIST TYPE VIMSELLIST,
    LS_EXCL    TYPE VIMEXCLFUN,

    LT_SELLIST TYPE TABLE OF VIMSELLIST,
    LT_EXCL    TYPE TABLE OF VIMEXCLFUN
    .

  LS_SELLIST-VIEWFIELD  = 'REPID'.
  LS_SELLIST-AND_OR     = 'AND'.
  LS_SELLIST-OPERATOR   = 'EQ'.
  LS_SELLIST-VALUE      = P_REPID.
  APPEND LS_SELLIST TO LT_SELLIST.

  CLEAR: LS_SELLIST.
  LS_SELLIST-VIEWFIELD  = 'BUKRS'.
  LS_SELLIST-OPERATOR   = 'EQ'.
  LS_SELLIST-VALUE      = P_BUKRS.
  APPEND LS_SELLIST TO LT_SELLIST.

  LS_EXCL-FUNCTION      = 'ZDSTB'.
  APPEND LS_EXCL TO LT_EXCL.

  LS_EXCL-FUNCTION      = 'ZEDWD'.
  APPEND LS_EXCL TO LT_EXCL.

  CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
    EXPORTING
      ACTION                       = 'S'
      VIEW_NAME                    = 'ZVI_BM_RPC_WC'
    TABLES
      DBA_SELLIST                  = LT_SELLIST
      EXCL_CUA_FUNCT               = LT_EXCL
    EXCEPTIONS
      CLIENT_REFERENCE             = 1
      FOREIGN_LOCK                 = 2
      INVALID_ACTION               = 3
      NO_CLIENTINDEPENDENT_AUTH    = 4
      NO_DATABASE_FUNCTION         = 5
      NO_EDITOR_FUNCTION           = 6
      NO_SHOW_AUTH                 = 7
      NO_TVDIR_ENTRY               = 8
      NO_UPD_AUTH                  = 9
      ONLY_SHOW_ALLOWED            = 10
      SYSTEM_FAILURE               = 11
      UNKNOWN_FIELD_IN_DBA_SELLIST = 12
      VIEW_NOT_FOUND               = 13
      MAINTENANCE_PROHIBITED       = 14
      OTHERS                       = 15.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.
