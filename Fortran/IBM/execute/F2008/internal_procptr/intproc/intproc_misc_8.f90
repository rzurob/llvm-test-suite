!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_misc_8.f
!*  TEST CASE TITLE          :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May 02, 2011
!*  ORIGIN                     : Compiler Development IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Internal procedure as actual argument or procedure target
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : CMVC Feature number 303977
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
!*  Miscellaneous Test  --  
!*    intrisic assignment on object of derived type with 
!*    procedure pointer component 
!*   
!*  
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
    BLOCK
    TYPE(DT(:)), allocatable :: ttt
    Arg%int_ptr => intsub
    ttt = Arg
    CALL ttt%int_ptr()
    SELECT TYPE ( Arg )
    TYPE IS (DT(*))
      Arg = ttt
    END SELECT
    END BLOCK
  END IF

  CONTAINS
    SUBROUTINE intsub(Arg)
      CLASS(DT(*)) :: Arg
      Arg%Id = - Arg%Id
    END SUBROUTINE
  END SUBROUTINE

  END MODULE

  PROGRAM intproc_misc_8
  USE M
  TYPE(DT(:)), allocatable :: ttt

  DO i = 1, 100
    ALLOCATE(tt, SOURCE=DT(i)(i, NULL()))
    ttt = tt
    CALL Modsub(ttt)
    IF (ANY(ttt%Id .NE. -i)) ERROR STOP 11
    IF (.NOT. ASSOCIATED(ttt%int_ptr)) ERROR STOP 12
    DEALLOCATE(tt)
  END DO

  END


