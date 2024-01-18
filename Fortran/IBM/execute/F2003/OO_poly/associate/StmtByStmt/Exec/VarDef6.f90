! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 22, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*   Variable Definition Context on non variable selector
!*   - Namelist
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM VarDef6
  IMPLICIT NONE
  INTEGER :: As=1
  NAMELIST /NList/ As
  CHARACTER(8) :: C(3)=(/" &NLIST"," AS=2  "," /     "/)

  ASSOCIATE ( As => 0 )

    READ(C, NML=NList)
    WRITE(*, NML=NList)
    IF (As .NE. 0 ) STOP 11

  END ASSOCIATE
  IF (As .NE. 2 )   STOP 12


  END

