!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May 04, 2011
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
!*   Access local variable through data pointer
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  CONTAINS

  SUBROUTINE msub(iarg)
  BLOCK
  INTEGER, TARGET :: iii(1000)
  INTEGER, POINTER :: Ptr(:)
    ptr => iii
    iii = -Iarg
    !CALL set(iii, Iarg)
    CALL sub(set,ptr, Iarg)
    IF (ANY(iii .NE. Iarg)) ERROR STOP 13
    !CALL Check(iii, Iarg)
    CALL sub(Check, ptr, Iarg)
  END BLOCK
  CONTAINS

    SUBROUTINE set(arr, iarg)
    INTEGER, POINTER :: arr(:)
      arr = iarg
    END SUBROUTINE

    SUBROUTINE check(arr, iarg)
    INTEGER, POINTER :: arr(:)
      IF (ANY(arr .NE. Iarg)) ERROR STOP 12
    END SUBROUTINE

    SUBROUTINE sub(proc, arr, iarg)
    PROCEDURE(set) :: proc
    INTEGER, POINTER :: arr(:)
      CALL proc(Arr, Iarg)
    END SUBROUTINE

  END SUBROUTINE

  END MODULE

  SUBROUTINE Esub(iarg)

  BLOCK
  INTEGER, TARGET :: iii(1000)
  INTEGER, POINTER :: Ptr(:)
    ptr => iii
    iii = -Iarg
    !CALL set(iii, Iarg)
    CALL sub(set,ptr, Iarg)
    IF (ANY(iii .NE. Iarg)) ERROR STOP 13
    !CALL Check(iii, Iarg)
    CALL sub(Check, ptr, Iarg)
  END BLOCK

  CONTAINS

    SUBROUTINE set(arr, iarg)
    INTEGER, POINTER :: arr(:)
      arr = iarg
    END SUBROUTINE

    SUBROUTINE check(arr, iarg)
    INTEGER, POINTER :: arr(:)
      IF (ANY(arr .NE. Iarg)) ERROR STOP 12
    END SUBROUTINE

    SUBROUTINE sub(proc, arr, iarg)
    PROCEDURE(set) :: proc
    INTEGER, POINTER :: arr(:)
      CALL proc(Arr, Iarg)
    END SUBROUTINE

  END SUBROUTINE


  PROGRAM  intproc_misc_13
  USE M
  EXTERNAL Esub

  DO I = 1, 10000
    CALL Msub(I)
    CALL Esub(I)
  END DO

  END

