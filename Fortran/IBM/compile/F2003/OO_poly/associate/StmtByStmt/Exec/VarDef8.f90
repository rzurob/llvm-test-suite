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
! %POSTCMD: tcomp VarDef8.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : VarDef8
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
!*   - IOSTAT, IOMSG
!*
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM VarDef8
  IMPLICIT NONE

  ASSOCIATE ( IOSTAT => (/1,2,3/), IOMSG => "ok" )
   WRITE(*, FMT=*, IOSTAT=IOSTAT(1), IOMSG=IOMSG) 1
  END ASSOCIATE

  END

