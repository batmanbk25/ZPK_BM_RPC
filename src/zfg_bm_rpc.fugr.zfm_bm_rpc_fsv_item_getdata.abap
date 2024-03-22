FUNCTION ZFM_BM_RPC_FSV_ITEM_GETDATA.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_ERGSL) TYPE  ERGSL
*"----------------------------------------------------------------------
  DATA:
    LS_NODE_ID  TYPE FIBS_BS_NODE_ID,
    LT_ACCT     TYPE FIBS_ACCT_INTERVAL_TAB.

  READ TABLE GT_FAGL_011SC INTO DATA(LS_FSV_SC)
    WITH KEY ERGSL = I_ERGSL.
  CHECK SY-SUBRC IS INITIAL.
  LS_NODE_ID-TYPE = 'P'.
  LS_NODE_ID-POS_ID = LS_FSV_SC-SETNR.

  CALL FUNCTION 'FI_BS_POS_GET_ALL_ACCOUNTS'
    EXPORTING
      NODE_ID       = LS_NODE_ID
    TABLES
      ACCT_TAB      = LT_ACCT
    EXCEPTIONS
      WRONG_TYPE    = 1
      POS_NOT_FOUND = 2
      OTHERS        = 3.

**Table FAGLFLEXT:
**RYEAR (Year) = Năm chạy báo cáo
**RPMAX (Period) = Tháng hiển thị trên báo cáo
**RRCTY (Record Type) = 0, 2
**RACCT (Account number) = Tài khoản ở chỉ tiêu 10002 cây ZHPA
**RBUKRS (Company Code) = Company Code của cột hiển thị
**Hiển thị số tổng của cột Total LC tương ứng với tháng
**(ví dụ Tháng 1 lấy số ở HSL01) ở cột Chi phí
*  IF LT_ACCT IS NOT INITIAL.
*    LOOP AT GT_FAGLFLEXT INTO DATA(LS_FAGLFLEXT)
*      WHERE RYEAR = LS_FCONTEXT-BEMON(4)
*        AND RBUKRS = LS_FCONTEXT-WERKS
**      AND RPMAX BETWEEN LS_FCONTEXT-BEMON+4(2) AND LS_FCONTEXT-ENMON+4(2)
*      .
**    LOOP AT LT_FAGL_FSV TRANSPORTING NO FIELDS
**      WHERE VONKT <= LS_FAGLFLEXT-RACCT
**      AND BISKT >= LS_FAGLFLEXT-RACCT.
*      LOOP AT LT_ACCT TRANSPORTING NO FIELDS
*        WHERE ACCT_FROM <= LS_FAGLFLEXT-RACCT
*        AND ACCT_TO >= LS_FAGLFLEXT-RACCT.
*        LW_BEMON = LS_FCONTEXT-BEMON.
*        WHILE LW_BEMON <= LS_FCONTEXT-ENMON.
*          LW_FNAME = 'HSL' && LW_BEMON+4(2).
*          ASSIGN COMPONENT LW_FNAME OF STRUCTURE LS_FAGLFLEXT TO <LF_HSL>.
*          IF SY-SUBRC IS INITIAL.
*            LW_DMBTR = LW_DMBTR + <LF_HSL>.
*          ENDIF.
*          LW_BEMON = LW_BEMON + 1.
*        ENDWHILE.
*        EXIT.
*      ENDLOOP.
*    ENDLOOP.
*  ENDIF.
*
*  ASSIGN COMPONENT IS_RPC_LF-FNAME OF STRUCTURE CS_RPC_D
*    TO <LF_COLVAL>.
*  IF SY-SUBRC IS INITIAL.
*    IF LW_DMBTR IS INITIAL.
*      <LF_COLVAL> = 0.
*      CONDENSE <LF_COLVAL>.
*    ELSE.
*      WRITE LW_DMBTR TO <LF_COLVAL>
*        CURRENCY LS_FAGLFLEXT-RTCUR NO-GROUPING NO-SIGN.
*      CONDENSE <LF_COLVAL>.
**      IF LW_DMBTR < 0.
**        <LF_COLVAL> = '-' && <LF_COLVAL>.
**      ENDIF.
*    ENDIF.
*  ENDIF.

ENDFUNCTION.
