*----------------------------------------------------------------------*
***INCLUDE LZFG_BM_RPCO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'ZGS_0100'.
  SET TITLEBAR 'ZGT_0100'.
  CALL FUNCTION 'ZFM_SCR_PBO'
    EXPORTING
      I_CPROG            = SY-REPID
      I_SET_LIST_VALUES  = 'X'
      I_SET_LIST_DEFAULT = SPACE.

  PERFORM 0100_PBO.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'ZGS_0200'.
  SET TITLEBAR 'ZGT_0200'.
  CALL FUNCTION 'ZFM_SCR_PBO'
    EXPORTING
      I_CPROG = SY-REPID
      I_DYNNR = SY-DYNNR.

  CASE ZST0_BM_RPC_DIMSET-FIGTY.
    WHEN '1' OR '2'.
      GS_FIGTY_SCR = '0210'.
    WHEN '3'.
      GS_FIGTY_SCR = '0220'.
    WHEN OTHERS.
      GS_FIGTY_SCR = '0230'.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0210  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0210 OUTPUT.

  CALL FUNCTION 'ZFM_SCR_PBO'
    EXPORTING
      I_CPROG            = SY-REPID
      I_DYNNR            = SY-DYNNR
      I_SET_LIST_VALUES  = 'X'
      I_SET_LIST_DEFAULT = SPACE.

  PERFORM 0210_PBO.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0220  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0220 OUTPUT.

  CALL FUNCTION 'ZFM_SCR_PBO'
    EXPORTING
      I_CPROG            = SY-REPID
      I_DYNNR            = '0220' "SY-DYNNR
      I_SET_LIST_VALUES  = 'X'
      I_SET_LIST_DEFAULT = SPACE.

  PERFORM 0220_PBO.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0300 OUTPUT.
  SET PF-STATUS 'ZGS_0300'.
  SET TITLEBAR 'ZGT_0300'.
  CALL FUNCTION 'ZFM_SCR_PBO'
    EXPORTING
      I_CPROG            = SY-REPID
      I_SET_LIST_VALUES  = 'X'
      I_SET_LIST_DEFAULT = SPACE.

  PERFORM 0300_PBO.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0310  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0310 OUTPUT.
  SET PF-STATUS 'ZGS_0310'.
  SET TITLEBAR 'ZGT_0300'.
  CALL FUNCTION 'ZFM_SCR_PBO'
    EXPORTING
      I_CPROG            = SY-REPID
      I_SET_LIST_VALUES  = 'X'
      I_SET_LIST_DEFAULT = SPACE.

  PERFORM 0310_PBO.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0320  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0320 OUTPUT.
  SET PF-STATUS 'ZGS_0310'.
  SET TITLEBAR 'ZGT_0300'.
  CALL FUNCTION 'ZFM_SCR_PBO'
    EXPORTING
      I_CPROG            = SY-REPID
      I_SET_LIST_VALUES  = 'X'
      I_SET_LIST_DEFAULT = SPACE.

  PERFORM 0320_PBO.

  IF SY-UCOMM = 'DUMMY'.
    READ TABLE GS_RPC_IN_F-DATA INTO DATA(LS_DATA)
      WITH KEY ROWPOS = ZST0_BM_RPC_DIMSET-ROWNO.
    IF SY-SUBRC IS INITIAL.
      ZST0_BM_RPC_DIMSET-CSTYLE = LS_DATA-LSTYLE.
    ENDIF.
  ENDIF.

ENDMODULE.
