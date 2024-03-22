*******************************************************************
*   System-defined Include-files.                                 *
*******************************************************************
  INCLUDE LZFG_BM_RPCTOP.                    " Global Declarations
  INCLUDE LZFG_BM_RPCUXX.                    " Function Modules

*******************************************************************
*   User-defined Include-files (if necessary).                    *
*******************************************************************
* INCLUDE LZFG_BM_RPCF...                    " Subroutines
* INCLUDE LZFG_BM_RPCO...                    " PBO-Modules
* INCLUDE LZFG_BM_RPCI...                    " PAI-Modules
* INCLUDE LZFG_BM_RPCE...                    " Events
* INCLUDE LZFG_BM_RPCP...                    " Local class implement.
* INCLUDE LZFG_BM_RPCT99.                    " ABAP Unit tests

  INCLUDE LZFG_BM_RPCF01.

  INCLUDE LZFG_BM_RPCO01.

  INCLUDE LZFG_BM_RPCI01.

  INCLUDE LZFG_BM_RPCI02.

  LOAD-OF-PROGRAM.
    PERFORM 0000_INIT_PROC.
