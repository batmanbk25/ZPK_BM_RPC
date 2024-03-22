*&---------------------------------------------------------------------*
*& Report ZPG_BM_RPCW
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

INCLUDE ZIN_BM_RPCWTOP                          .    " Global Data

* INCLUDE ZIN_BM_RPCWO01                          .  " PBO-Modules
* INCLUDE ZIN_BM_RPCWI01                          .  " PAI-Modules
INCLUDE ZIN_BM_RPCWF01                          .  " FORM-Routines

START-OF-SELECTION.
  PERFORM MAIN_PROC.
