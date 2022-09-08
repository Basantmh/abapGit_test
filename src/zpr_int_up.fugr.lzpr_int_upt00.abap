*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPR_INT_UP......................................*
DATA:  BEGIN OF STATUS_ZPR_INT_UP                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPR_INT_UP                    .
CONTROLS: TCTRL_ZPR_INT_UP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPR_INT_UP                    .
TABLES: ZPR_INT_UP                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
