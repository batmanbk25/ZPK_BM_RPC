*----------------------------------------------------------------------*
***INCLUDE LZFG_BM_RPCI01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

*  CALL METHOD GO_SI_ALV->CHECK_CHANGED_DATA.

  CASE SY-UCOMM.
    WHEN 'SAVE'.
      PERFORM 0100_SAVE.
    WHEN 'DELROW'.
      PERFORM 0100_DELETE_ROWS.
    WHEN 'ADDHROW'.
      PERFORM 0100_ADD_HEADER_ROWS.
    WHEN 'ADDROWS'.
      PERFORM 0100_ADD_ROWS.
    WHEN 'DELCOL'.
      PERFORM 0100_DELETE_COLS.
    WHEN 'ADDHCOL'.
      PERFORM 0100_ADD_HIDDEN_COLS.
    WHEN 'TEMPLATE'.
      PERFORM 0100_CHANGE_TEMPLATE.
    WHEN 'BACK'.
      PERFORM 0100_BACK.
    WHEN 'EXIT'.
      PERFORM 0100_EXIT.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.

  CALL FUNCTION 'ZFM_SCR_SIMPLE_FC_PROCESS'.
  IF GO_SI_COND IS BOUND.
    CALL METHOD GO_SI_COND->CHECK_CHANGED_DATA.
  ENDIF.

  CASE SY-UCOMM.
    WHEN 'FIGURE_TYPE'.
      PERFORM 0200_CHANGE_FIGURE_TYPE.
    WHEN 'CANCEL'.
      PERFORM 0200_CANCEL.
    WHEN 'FC_LSTYLE'.
      PERFORM 0200_FC_LSTYLE.
    WHEN 'OK'.
      CALL FUNCTION 'ZFM_SCR_PAI'
        EXPORTING
          I_CPROG = SY-REPID
          I_DYNNR = SY-DYNNR.
      PERFORM 0200_OK.
    WHEN 'MOVE_ALL_RIGHT'.
      PERFORM 0210_FC_MOVE_ALL_RIGHT.
    WHEN 'MOVE_RIGHT'.
      PERFORM 0210_FC_MOVE_RIGHT.
    WHEN 'MOVE_LEFT'.
      PERFORM 0210_FC_MOVE_LEFT.
    WHEN 'MOVE_ALL_LEFT'.
      PERFORM 0210_FC_MOVE_ALL_LEFT.
    WHEN 'PARS'.
      PERFORM 0220_PARSE_FORMULA.
    WHEN 'FC_SRCTAB'.
      PERFORM 0210_FC_SRCTAB.
    WHEN 'ADDCOND'.
      PERFORM 0210_ADDCOND.
    WHEN OTHERS.
      CALL FUNCTION 'ZFM_SCR_PAI'
        EXPORTING
          I_CPROG = SY-REPID
          I_DYNNR = SY-DYNNR.
  ENDCASE.
  CLEAR: SY-UCOMM.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0300 INPUT.

  CALL METHOD GO_F_ALV->CHECK_CHANGED_DATA.

  CASE SY-UCOMM.
    WHEN 'SAVE'.
      PERFORM 0300_SAVE_FORM_STRUCTURE.
    WHEN 'EXECUTE'.
      PERFORM 0300_EXECUTE.
    WHEN 'BACK'.
      CALL METHOD GO_F_ALV->FREE.
      CALL METHOD GO_F_CON->FREE.
      FREE: GO_F_ALV, GO_F_CON.
      LEAVE TO SCREEN 0.
      CALL METHOD CL_GUI_CFW=>FLUSH.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0310 INPUT.

  CALL METHOD GO_F_ALV->CHECK_CHANGED_DATA.

  CASE SY-UCOMM.
    WHEN 'SAVE'.
      PERFORM 0300_SAVE_FORM_STRUCTURE.
    WHEN 'ADDROWS'.
      PERFORM 0310_ADD_ROWS.
    WHEN 'BACK'.
      CALL METHOD GO_F_ALV->FREE.
      CALL METHOD GO_F_CON->FREE.
      FREE: GO_F_ALV, GO_F_CON.
      LEAVE TO SCREEN 0.
      CALL METHOD CL_GUI_CFW=>FLUSH.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0320 INPUT.
  CHECK SY-UCOMM <> 'DUMMY'.
  CALL METHOD GO_F_ALV->CHECK_CHANGED_DATA.

  PERFORM 0320_UPDATE_TEXT_TO_ALV.

  CASE SY-UCOMM.
    WHEN 'SAVE'.
      PERFORM 0300_SAVE_FORM_STRUCTURE.
    WHEN 'ADDROWS'.
      PERFORM 0310_ADD_ROWS.
    WHEN 'BACK'.
      CALL METHOD GO_F_ALV->FREE.
      CALL METHOD GO_F_TEXT->FREE.
      CALL METHOD GO_F_CON->FREE.
      CALL METHOD GO_TEXT_CON->FREE.
      FREE: GO_F_ALV, GO_F_CON, GO_TEXT_CON, GO_F_TEXT.
      LEAVE TO SCREEN 0.
      CALL METHOD CL_GUI_CFW=>FLUSH.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  0200_CANCEL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE 0200_CANCEL INPUT.
  PERFORM 0200_CANCEL.

ENDMODULE.
