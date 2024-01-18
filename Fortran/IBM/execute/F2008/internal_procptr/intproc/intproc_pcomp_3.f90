!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_pcomp_3.f
!*
!*  DATE                       : April 29 2011
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
!*  Test procedure pointer component --
!*    procedure pointer component with The PASS attribute
!*  (388523/ 388927/390175)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT(l)
    INTEGER, LEN :: l
    INTEGER :: arr(l)
    PROCEDURE(INTSUB), POINTER, PASS :: procptr
  END TYPE

  INTEGER :: iarr(1000)

  CONTAINS
    SUBROUTINE intsub(T, iarg)
    CLASS(DT(*)) :: T
    IF ( ANY(iarr .NE. iarg) ) ERROR STOP 11
    !IF ( .NOT. Associated(T%procptr, intsub1)) ERROR STOP 22
    IF ( .NOT. Associated(T%procptr)) ERROR STOP 22
    iarr = iarg + 1
    CALL T%procptr(iarg+1)
    END SUBROUTINE

    !SUBROUTINE intsub1(T, iarg)
    RECURSIVE SUBROUTINE intsub1(T, iarg)
    CLASS(DT(*)) :: T
    IF ( ANY(iarr .NE. iarg) ) ERROR STOP 12
    !IF ( .NOT. Associated(T%procptr, intsub1)) ERROR STOP 23  !388927 is considered illegal
    IF ( .NOT. Associated(T%procptr)) ERROR STOP 23
    iarr = iarg+1
    END SUBROUTINE

  END MODULE

  PROGRAM intproc_pcomp_3
  USE M

  TYPE(DT(100)) :: T

  DO i = 1, 100
    iarr = i
    T = DT(100)(i, intsub1)
    IF ( .NOT. Associated(T%procptr, intsub1)) ERROR STOP 21
    CALL intsub(T, i)
    IF ( ANY(iarr .NE. i+2) ) ERROR STOP 15
  END DO

  END


