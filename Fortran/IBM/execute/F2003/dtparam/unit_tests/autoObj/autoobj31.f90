!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 30, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DTPARAM: Automatic objects
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 333331
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  The length parameter depends on a V through use association
!*
!*  ()
!*
!234567893123456789312345678931234567893123456789312345678931234567890

  MODULE M
  INTEGER ::  N=10
  END MODULE

  PROGRAM autoobj31
  USE M

  CALL sub()
  CONTAINS

  SUBROUTINE Sub()

  TYPE dt(l)
     INTEGER, LEN :: l
     INTEGER      :: arr(l)=-1
  END TYPE

  TYPE(dt(N*2)) b

  PRINT*, size(b%arr)
  PRINT*, b%arr

  IF (b%l         .NE. 20)  STOP 11
  IF (SIZE(b%arr) .NE. 20)  STOP 12
  IF (ANY(b%arr   .NE. -1)) STOP 13

  END SUBROUTINE
  END

