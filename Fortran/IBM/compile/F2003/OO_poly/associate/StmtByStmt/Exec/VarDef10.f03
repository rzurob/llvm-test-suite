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
!*   - ALLOCATE:IOSTAT, IOMSG
!*
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM VarDef10
  IMPLICIT NONE
  INTEGER :: J(2)
  INTEGER, POINTER :: Obj(:)

  ASSOCIATE ( STAT => J((/1/)), ERRMSG => "ok" )
    ALLOCATE(Obj(3), STAT=STAT(1))
    DEALLOCATE(Obj, ERRMSG=ERRMSG)
  END ASSOCIATE

  END

