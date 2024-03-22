*&---------------------------------------------------------------------*
*& Report ZPG_CPOC_MASTER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPG_CPOC_MASTER.

PARAMETERS:
  P_DATA   TYPE XMARK RADIOBUTTON GROUP RPTY DEFAULT 'X',
  P_REPORT TYPE XMARK RADIOBUTTON GROUP RPTY.

START-OF-SELECTION.
  PERFORM MAIN_PROC.

*&---------------------------------------------------------------------*
*&      Form  MAIN_PROC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAIN_PROC .
  CASE 'X'.
    WHEN P_DATA.
      CALL FUNCTION 'VIEWCLUSTER_MAINTENANCE_CALL'
        EXPORTING
          VIEWCLUSTER_NAME                   = 'ZVC_BM_RPC_D'
*         START_OBJECT                       = '          '
          MAINTENANCE_ACTION                 = 'U'
*         READ_KIND                          = ' '
*         SHOW_SELECTION_POPUP               = ' '
*         CORR_NUMBER                        = ' '
*         NO_WARNING_FOR_CLIENTINDEP         = ' '
*         RFC_DESTINATION                    = ' '
*         SUPPRESS_WA_POPUP                  = ' '
*       TABLES
*         DBA_SELLIST                        =
*         DBA_SELLIST_CLUSTER                =
*         EXCL_CUA_FUNCT_ALL_OBJECTS         =
*         EXCL_CUA_FUNCT_CLUSTER             =
*         DPL_SELLIST_FOR_START_OBJECT       =
*       EXCEPTIONS
*         CLIENT_REFERENCE                   = 1
*         FOREIGN_LOCK                       = 2
*         VIEWCLUSTER_NOT_FOUND              = 3
*         VIEWCLUSTER_IS_INCONSISTENT        = 4
*         MISSING_GENERATED_FUNCTION         = 5
*         NO_UPD_AUTH                        = 6
*         NO_SHOW_AUTH                       = 7
*         OBJECT_NOT_FOUND                   = 8
*         NO_TVDIR_ENTRY                     = 9
*         NO_CLIENTINDEP_AUTH                = 10
*         INVALID_ACTION                     = 11
*         SAVING_CORRECTION_FAILED           = 12
*         SYSTEM_FAILURE                     = 13
*         UNKNOWN_FIELD_IN_DBA_SELLIST       = 14
*         MISSING_CORR_NUMBER                = 15
*         OTHERS                             = 16
                .
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.

    WHEN P_REPORT.
      SUBMIT ZPG_CPOC_FI_REPORT VIA SELECTION-SCREEN .
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
