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
!*  Variables in different scoping units whith the same name
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  INTEGER :: iii(1000)

  CONTAINS

  SUBROUTINE msub1(iarg)
  INTEGER :: iii(1000)
  PROCEDURE(), POINTER :: procptr
    iii = iarg + 111
    procptr => set
    CALL procptr(Iarg)
    procptr => check
    CALL procptr(Iarg)
  CONTAINS
    SUBROUTINE set(iarg)
      iii = Iarg
    END SUBROUTINE
    SUBROUTINE check(iarg)
      IF (ANY(iii .NE. Iarg)) ERROR STOP 11
    END SUBROUTINE
  END SUBROUTINE

  SUBROUTINE msub2(iarg)
  BLOCK
  INTEGER :: iii(1000)
  PROCEDURE(), POINTER :: procptr
    iii = iarg + 111
    procptr => set
    CALL procptr(Iarg)
    IF (ANY(iii .NE. Iarg+111)) ERROR STOP 13
    procptr => check
    CALL procptr(Iarg)
  END BLOCK
  CONTAINS
    SUBROUTINE set(iarg)
      iii = Iarg+3
    END SUBROUTINE
    SUBROUTINE check(iarg)
      IF (ANY(iii .NE. Iarg+3)) ERROR STOP 12
    END SUBROUTINE
  END SUBROUTINE

  SUBROUTINE msub3(iarg)
  INTEGER :: iii(1000)
  BLOCK
  INTEGER :: iii(1000)
  PROCEDURE(), POINTER :: procptr
    iii = iarg + 111
    procptr => set
    CALL procptr(Iarg)
    IF (ANY(iii .NE. Iarg+111)) ERROR STOP 14
    procptr => check
    CALL procptr(Iarg)
  END BLOCK
  CONTAINS
    SUBROUTINE set(iarg)
      iii = Iarg+4
    END SUBROUTINE
    SUBROUTINE check(iarg)
      IF (ANY(iii .NE. Iarg+4)) ERROR STOP 15
    END SUBROUTINE
  END SUBROUTINE


  END MODULE

  PROGRAM  intproc_misc_12
  USE M

  DO I = 1, 10000
    iii = -I
    CALL Msub1(I)
    IF (ANY(iii .NE. -I)) ERROR STOP 16
    CALL Msub2(I)
    IF (ANY(iii .NE. I+3)) ERROR STOP 17
    CALL Msub3(I)
    IF (ANY(iii .NE. I+3)) ERROR STOP 18
  END DO

  END

