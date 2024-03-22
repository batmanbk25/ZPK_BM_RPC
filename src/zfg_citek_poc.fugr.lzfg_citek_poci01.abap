*----------------------------------------------------------------------*
***INCLUDE LZFG_CITEK_POCI01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  FC_DESIGN_TABLE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE FC_DESIGN_TABLE INPUT.
  CHECK SY-UCOMM = 'ZDSTB'.
  PERFORM PROCESS_FC_DESIGN_TABLE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  FC_INPUT_MANUAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE FC_INPUT_MANUAL INPUT.
  CHECK SY-UCOMM = 'ZDSTB'.
  PERFORM PROCESS_FC_INPUT_MANUAL.
ENDMODULE.

*{   INSERT         DEVK909644                                        1
*&---------------------------------------------------------------------*
*&      Module  FC_EDIT_TEXT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE FC_EDIT_TEXT INPUT.
*  CHECK SY-UCOMM = 'ZEDWB'.
  CHECK SY-UCOMM = 'ZEDWD'.
  PERFORM PROCESS_FC_EDIT_DF.
ENDMODULE.
*}   INSERT

*{   INSERT         DEVK909644                                        2
*&---------------------------------------------------------------------*
*&      Module  FC_EDIT_TEXT_WC  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE FC_EDIT_TEXT_WC INPUT.
  CASE SY-UCOMM.
    WHEN 'ZVWDF'. "Display default
      PERFORM PROCESS_FC_DISP_DF.
    WHEN 'ZEDWB'. "Update wording
      PERFORM PROCESS_FC_EDIT_WF.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*}   INSERT
