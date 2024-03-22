*----------------------------------------------------------------------*
***INCLUDE LZFG_BM_RPCI02.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0220  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0220 INPUT.
  CALL FUNCTION 'ZFM_SCR_PAI'
    EXPORTING
      I_CPROG = SY-REPID
      I_DYNNR = SY-DYNNR.

  CALL FUNCTION 'ZFM_SCR_GET_CURSOR'
    EXPORTING
      I_DYNNR = SY-DYNNR.

  CASE SY-UCOMM.
    WHEN 'KADD'.
      PERFORM 0220_INSERT_ELEMENT USING  '+'.
    WHEN 'KSUB'.
      PERFORM 0220_INSERT_ELEMENT USING  '-'.
    WHEN 'KMUL'.
      PERFORM 0220_INSERT_ELEMENT USING  '*'.
    WHEN 'KDIV'.
      PERFORM 0220_INSERT_ELEMENT USING  '/'.
    WHEN 'CLUP'.
      PERFORM 0220_INSERT_ELEMENT USING  '('.
    WHEN 'CLCS'.
      PERFORM 0220_INSERT_ELEMENT USING  ')'.
    WHEN 'ADD0'.
      PERFORM 0220_INSERT_ELEMENT USING  '0'.
    WHEN 'ADD1'.
      PERFORM 0220_INSERT_ELEMENT USING  '1'.
    WHEN 'ADD2'.
      PERFORM 0220_INSERT_ELEMENT USING  '2'.
    WHEN 'ADD3'.
      PERFORM 0220_INSERT_ELEMENT USING  '3'.
    WHEN 'ADD4'.
      PERFORM 0220_INSERT_ELEMENT USING  '4'.
    WHEN 'ADD5'.
      PERFORM 0220_INSERT_ELEMENT USING  '5'.
    WHEN 'ADD6'.
      PERFORM 0220_INSERT_ELEMENT USING  '6'.
    WHEN 'ADD7'.
      PERFORM 0220_INSERT_ELEMENT USING  '7'.
    WHEN 'ADD8'.
      PERFORM 0220_INSERT_ELEMENT USING  '8'.
    WHEN 'ADD9'.
      PERFORM 0220_INSERT_ELEMENT USING  '9'.
    WHEN 'ADDP'.
      PERFORM 0220_INSERT_ELEMENT USING  '.'.
  ENDCASE.

ENDMODULE.
