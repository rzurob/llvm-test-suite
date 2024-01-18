!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_misc_10.f
!*
!*  DATE                       : May 03, 2011
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
!*    procedure pointer component, and with the VALUE attribute
!*
!*  The host instance of an internal procedure that is
!*  invoked via a dummy procedure or procedure pointer is the host
!*  instance of the associating entity from when the
!*  argument association or pointer association was established
!*  (388639)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT
    INTEGER :: Id(10)
    PROCEDURE(modsub), POINTER, NOPASS :: int_ptr
  END TYPE

  TYPE(DT), allocatable :: tt
  PROCEDURE(modsub), POINTER :: mod_procptr

  CONTAINS

  RECURSIVE SUBROUTINE Modsub(Arg, N)
  TYPE(DT), VALUE :: Arg

  IF ( ASSOCIATED(Arg%int_ptr)) THEN
    CALL Arg%int_ptr(Arg, N)
    print*, Arg%Id
    IF (ANY(Arg%Id .NE. N)) ERROR STOP 11  !<-- note that the host instance is the first
                                           !<-- instance of modsub. No effect here.
    !IF (.NOT. ASSOCIATED(Arg%int_ptr, intsub)) ERROR STOP 12
    ! the host instance of intsub now is diffenret from the one when
    ! "Arg%int_ptr => intsub" is done
    IF (      ASSOCIATED(Arg%int_ptr, intsub)) ERROR STOP 12
    IF (.NOT. ASSOCIATED(Arg%int_ptr, mod_procptr)) ERROR STOP 13
  ELSE
    Arg%int_ptr => intsub
    mod_procptr => intsub
    CALL modsub(Arg, N)
  END IF

  CONTAINS
    SUBROUTINE intsub(Int_Arg, N)
      TYPE(DT), VALUE :: Int_Arg
      Arg%Id = - Arg%Id
    END SUBROUTINE
  END SUBROUTINE

  END MODULE

  PROGRAM intproc_misc_10
  USE M

  DO i = 1, 100
    ALLOCATE(tt, SOURCE=DT(i, NULL()))
    CALL Modsub(tt, i)
    DEALLOCATE(tt)
  END DO

  END


