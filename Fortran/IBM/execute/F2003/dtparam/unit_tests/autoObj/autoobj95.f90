!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 31, 2009
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
!*  Char comp and array dummy
!*  (RTO issue)
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM autoobj03

  TYPE base(l)
    INTEGER, LEN :: l
    CHARACTER(l) :: c="12345"
  END TYPE

  TYPE dt(l)
    INTEGER, LEN :: l
    TYPE(base(l)) :: arr(l:l+l-1)
  END TYPE

  CALL sub(2)
  CONTAINS

  SUBROUTINE Sub(N)

  TYPE(dt(n*2)) b(n-1)

  IF (b%l                .NE. 4)  STOP 11
  IF (b(1)%arr%l         .NE. 4)  STOP 12
  IF (SIZE(b(1)%arr)     .NE. 4)  STOP 13
  IF (lbound(b(1)%arr,1) .NE. 4)  STOP 14
  IF (ubound(b(1)%arr,1) .NE. 7)  STOP 15
  IF (len(b(1)%arr(1)%c) .NE. 4)  STOP 16

  do i = 1, n-1
    b(i)%arr%c = '43215'
  end do

  IF (ANY(b(1)%arr%c     .NE. "4321")) STOP 17
  IF (ANY(b%arr(2*n)%c     .NE. "4321")) STOP 18

  END SUBROUTINE
  END

