!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_misc_7.f
!*
!*  DATE                       : May 02, 2011
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
!*  Miscellaneous Test  --
!*    call type bound procedure with internal procdure argument
!*
!* (390794)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  INTEGER, TARGET :: mark

  TYPE :: DT(l)
    INTEGER, LEN :: l
    INTEGER :: Id(l)
  CONTAINS
    PROCEDURE :: TB => Modsub
  END TYPE

  TYPE(DT(:)), allocatable :: tt

  CONTAINS

  SUBROUTINE intsub(Arg)
    CLASS(DT(*)) :: Arg
    Arg%Id = - Arg%Id
  END SUBROUTINE

  RECURSIVE SUBROUTINE Modsub(Arg, Procptr)
  CLASS(DT(*)) :: Arg
  PROCEDURE(intsub), POINTER, INTENT(IN) :: procptr

  IF ( ASSOCIATED(procptr)) THEN
    CALL procptr(Arg)
  ELSE
    CALL Arg%TB(intsub)
  END IF

  END SUBROUTINE

  END MODULE

  PROGRAM intproc_misc_7
  USE M

  DO i = 1, 100
    ALLOCATE(tt, SOURCE=DT(i)(i))
    CALL tt%TB( NULL())
    IF (ANY(tt%Id .NE. -i)) ERROR STOP 11
    DEALLOCATE(tt)
  END DO

  END

