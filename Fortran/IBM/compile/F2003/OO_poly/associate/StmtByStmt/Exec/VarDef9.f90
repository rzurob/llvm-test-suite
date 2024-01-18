! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp VarDef9.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : VarDef9
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
!*   - INQUIRE:IOSTAT, IOMSG
!*
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM VarDef9
  IMPLICIT NONE
  INTEGER :: J(2)
  ASSOCIATE ( IOSTAT => J((/1/)), IOMSG => "ok" )
    INQUIRE(5, IOSTAT=IOSTAT, IOMSG=IOMSG)
  END ASSOCIATE

  END

