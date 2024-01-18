! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Misc29.f 
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
!*  TEST CASE NAME             : Misc29 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar. 09, 2005
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
!*   The procedure statment 
!*    (ICE-301114) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      INTEGER      :: Id = 0
    END TYPE
  END MODULE

  USE M

  TYPE(DT) :: V =  DT(-1)

  INTERFACE
    FUNCTION Fun(Arg)
      IMPORT DT
      TYPE(DT) :: Fun
      INTEGER, INTENT(IN) :: Arg
    END FUNCTION
  END INTERFACE

  PROCEDURE(Fun) :: Fun1

  CALL Sub(Fun1)

  CONTAINS

  SUBROUTINE Sub(Arg)
  PROCEDURE(Fun) :: Arg
    V = Arg(1) ! Iced here
    IF (V%ID .NE. 1 ) STOP 99
  END SUBROUTINE

  END

  FUNCTION Fun1(Arg)
  USE M
  TYPE(DT) :: Fun1
  INTEGER, INTENT(IN)  :: Arg
    Fun1 = DT(Arg )
  END FUNCTION


