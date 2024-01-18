!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_misc_9.f
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
!*    argument association on data object of derived type that has
!*    procedure pointer component
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT(l)
    INTEGER, LEN :: l
    INTEGER :: Id(l)
    PROCEDURE(modsub), POINTER :: int_ptr
  END TYPE

  TYPE(DT(:)), allocatable :: tt

  CONTAINS

  RECURSIVE SUBROUTINE Modsub(Arg)
  CLASS(DT(*)) :: Arg

  IF ( ASSOCIATED(Arg%int_ptr)) THEN
    CALL Arg%int_ptr()
  ELSE
    Arg%int_ptr => intsub
    CALL modsub(Arg)
  END IF

  CONTAINS
    SUBROUTINE intsub(Arg)
      CLASS(DT(*)) :: Arg
      Arg%Id = - Arg%Id
    END SUBROUTINE
  END SUBROUTINE

  END MODULE

  PROGRAM intproc_misc_9
  USE M

  DO i = 1, 100
    ALLOCATE(tt, SOURCE=DT(i)(i, NULL()))
    CALL Modsub(tt)
    IF (ANY(tt%Id .NE. -i)) ERROR STOP 11
    IF (.NOT. ASSOCIATED(tt%int_ptr)) ERROR STOP 12
    DEALLOCATE(tt)
  END DO

  END


