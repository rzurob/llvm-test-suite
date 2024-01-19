!*********************************************************************
!*  ===================================================================
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
!*    Recursinve internal function returning procedure pointer
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM intproc_misc_3
  PROCEDURE(), POINTER :: procptr
  PROCEDURE(), POINTER :: procptr1
  INTEGER :: mark


  DO i = 1, 100
    mark = 1
    procptr => intfunc(i)
    !IF ( .NOT. associated(procptr, intfunc)) ERROR STOP 12
   IF ( .NOT. associated(procptr, procptr1)) ERROR STOP 12
    procptr => null()
  END DO

  CONTAINS

    FUNCTION Ret_ptr(iarg)
    PROCEDURE(), POINTER :: ret_ptr
        ret_ptr => intfunc
        procptr1 => intfunc
    END FUNCTION

    RECURSIVE FUNCTION  intfunc( iarg)
    PROCEDURE(), POINTER :: intfunc
    INTEGER, SAVE :: V
    IF ( mark == 1) THEN
      V = 1
      mark = 2
      intfunc => Intfunc(V+1)
    ELSE
      V = V + 1
      IF ( V /= Iarg ) ERROR STOP 11
      IF ( V == 10) THEN
        intfunc => intfunc
        procptr1 => intfunc
      ELSE
        intfunc => ret_ptr(V)
      END IF
    END IF
    END FUNCTION
  END

