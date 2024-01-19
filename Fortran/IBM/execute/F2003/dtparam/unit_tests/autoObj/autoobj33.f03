!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 30, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DTPARAM: Automatic objects
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 333321
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
!*  the component is another derived type
!*
!*  ()
!*
!334567893333456789333345678933334567893333456789333345678933334567890

  MODULE M
  INTEGER :: N=2
  END MODULE

  PROGRAM autoobj33
  USE M

  CALL sub()

  CONTAINS

  SUBROUTINE Sub()

  TYPE base(l)
    INTEGER, LEN :: l
    CHARACTER(l) :: c="12345"
  END TYPE

  TYPE dt(l)
    INTEGER, LEN :: l
    TYPE(base(l)) :: arr(l:l+l-1)
  END TYPE

  TYPE(dt(n*2)) b

print*, b%l
print*, SIZE(b%arr)
print*, len(b%arr%c)

  IF (b%l         .NE. 4)  ERROR STOP 11
  IF (b%arr%l     .NE. 4)  ERROR STOP 12
  IF (SIZE(b%arr) .NE. 4)  ERROR STOP 13

  do i = 2*n, 2*2*n -1
    b%arr(i)%c = '1334'
  end do

  IF (ANY(b%arr%c  .NE. "1334")) ERROR STOP 14

  END SUBROUTINE
  END

