!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_arg_4.f
!*  TEST CASE TITLE          :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : April 21 2011
!*  ORIGIN                     : Compiler Development IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Internal procedure as actual argument or procedure target
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : CMVC Feature number 303977
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*
!*  Test the dummy procedure --
!*    An elemental efective argument may be associated with a dummy argument
!*    that is not elemental 
!*   
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  REAL :: R(1000)

  ABSTRACT INTERFACE 
    REAL FUNCTION rf(arg)
      REAL :: arg
    END FUNCTION
  END INTERFACE

  CONTAINS

  SUBROUTINE Intcallsub(proc, i)

  procedure(rf) :: proc
   R(i) = proc(REAL(i)) 
  END SUBROUTINE

  SUBROUTINE Intcheck(i)
    procedure(logical) :: precision_R4
    IF ( .NOT. precision_R4(SIN(REAL(i)), R(i))) ERROR STOP 11 
  END SUBROUTINE


  END MODULE
 
  PROGRAM intproc_arg_4
  USE M
  INTRINSIC SIN
  PROCEDURE(Intcallsub), POINTER :: procptr

  procptr => Intcallsub

  DO I=1, 1000
    CALL procptr(SIN, i)
    CALL Intcheck(i)
  END DO

  END

