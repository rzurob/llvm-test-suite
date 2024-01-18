! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Misc22.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc22
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 16, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*  
!*
!* (299886)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc22


  CALL Sub((/1,2,3/), 3)
  CONTAINS
  SUBROUTINE Sub(Arr, N)
  INTEGER :: Arr(*), N
    ASSOCIATE ( As => Arr(1:N) )
      IF (ANY(SHAPE(As) .NE. (/3/)) ) STOP 30
    END ASSOCIATE
  END SUBROUTINE
  END

