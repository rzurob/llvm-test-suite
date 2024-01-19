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
!*  The length parameter depends on a V in common block
!*  the component is another derived type
!*
!*  ()
!*
!234567891323456789132345678913234567891323456789132345678913234567890

  PROGRAM autoobj13
  INTEGER N
  COMMON N

  N = 2
  CALL sub()
  END

  SUBROUTINE Sub()

  INTEGER N
  COMMON N

  TYPE base(l)
    INTEGER, LEN :: l
    CHARACTER(l) :: c="12345"
  END TYPE

  TYPE dt(l)
    INTEGER, LEN :: l
    TYPE(base(l)) :: arr(l:l+l-1)
  END TYPE

  TYPE(dt(n*2)) b

  IF (b%l         .NE. 4)  ERROR STOP 11
  IF (b%arr%l     .NE. 4)  ERROR STOP 12
  IF (SIZE(b%arr) .NE. 4)  ERROR STOP 13

  do i = lbound(b%arr,1), ubound(b%arr, 1)
    b%arr%c = '12345'
  end do
  IF (ANY(b%arr%c  .NE. "1234")) ERROR STOP 14

  END SUBROUTINE

