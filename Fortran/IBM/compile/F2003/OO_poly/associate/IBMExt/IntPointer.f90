! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  IntPointer.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : IntPointer
!*
!*  DATE                       : Mar. 05, 2005
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
!*    The selector is an integer pointer
!*    (ICE-300793) - the decision is we do not support integer pointer.
!*                   change the tc be diagnostic.
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM IntPointer
  IMPLICIT NONE

  INTEGER :: ITemp, i
  POINTER  (IntPtr, ITemp)

  ASSOCIATE ( As => IntPtr  )
  END ASSOCIATE

  END
