!*********************************************************************
!*  ===================================================================
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
!*  host variables are not directly accessed.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  INTEGER :: museful (1000)
  END MODULE

  PROGRAM  intproc_misc_11
  INTEGER :: useful (1000)
  EXTERNAL :: extsub

  DO I = 1, 10000
    CALL extsub(intset, I)
    CALL extsub(intcheck, I)
    CALL extsub(intsetm, I)
    CALL extsub(intcheckm, I)
  END DO

  CONTAINS
  SUBROUTINE intsetm(iarg)
  USE M
    museful = iarg
  END SUBROUTINE

  SUBROUTINE intcheckm(iarg)
  USE M
    IF ( ANY(museful .NE. Iarg)) ERROR STOP 11
  END SUBROUTINE


  SUBROUTINE intset(iarg)
    useful = iarg
  END SUBROUTINE

  SUBROUTINE intcheck(iarg)
    IF ( ANY(useful .NE. Iarg)) ERROR STOP 12
  END SUBROUTINE

  END

  SUBROUTINE extsub(proc, iarg)
  EXTERNAL proc
    CALL proc(iarg)
  END SUBROUTINE
