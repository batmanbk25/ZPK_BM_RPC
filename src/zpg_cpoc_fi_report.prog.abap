*&---------------------------------------------------------------------*
*& Report  ZPG_CPOC_FI_REPORT
*&
*&---------------------------------------------------------------------*
*&  Created by: TuanBA
*&  Created date: 18/7/2019
*&---------------------------------------------------------------------*

REPORT ZPG_CPOC_FI_REPORT.
INCLUDE ZIN_CPOC_FI_REPORTTOP                   .    " global Data

* INCLUDE ZIN_CPOC_FI_REPORTO01                   .  " PBO-Modules
* INCLUDE ZIN_CPOC_FI_REPORTI01                   .  " PAI-Modules
INCLUDE ZIN_CPOC_FI_REPORTF01                   .  " FORM-Routines

AT SELECTION-SCREEN OUTPUT.
  PERFORM 1000_PBO.

AT SELECTION-SCREEN.
  PERFORM 1000_PAI.

INITIALIZATION.
  PERFORM 0000_INIT_PROC.

START-OF-SELECTION.
  PERFORM 0000_MAIN_PROC.
