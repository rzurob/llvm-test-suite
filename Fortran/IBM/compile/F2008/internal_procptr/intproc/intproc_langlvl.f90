!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_langlvl.f
!*
!*  DATE                       : April 21 2011
!*
!*  PRIMARY FUNCTIONS TESTED   : Internal procedure as actual argument or procedure target
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : CMVC Feature number 303977
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Langlvl test -- F03 does not support inter procedure as actual argument or
!*                  procedure pointer target
!*  ( 388915)
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM intproc_langlvl
  !EXTERNAL :: procptr
  !POINTER  :: Procptr
  PROCEDURE(), POINTER :: procptr

  CALL Intsub1(Intsub)
  CALL Intsub1(intfunc(intsub))

  procptr => intsub
  procptr => intfunc(intsub)

  CONTAINS

  SUBROUTINE Intsub()
  END SUBROUTINE

  SUBROUTINE Intsub1(proc)
  external proc
  END SUBROUTINE

  FUNCTION Intfunc(proc)
  external proc
  PROCEDURE(), POINTER :: Intfunc
    Intfunc => proc
  END FUNCTION

  END

