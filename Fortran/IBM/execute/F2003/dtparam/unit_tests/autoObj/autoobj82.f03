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
!*  the "base" type has array components
!*
!234567890123456789012345678901234567890123456789012345678901234567890

! It is now RTO issue! wait for Daniel's fix

  PROGRAM autoobj82
  CALL sub(4)

  CONTAINS

  SUBROUTINE Sub(N)

  TYPE base(l)
    INTEGER, LEN :: l
    integer :: j(l)=-1
    integer :: i(l)=1
  END TYPE

  TYPE dt(l)
    INTEGER, LEN :: l
    TYPE(base(l)) :: arr1(l:l)
    TYPE(base(l)) :: arr(1:l)
  END TYPE

  TYPE(dt(n)) b!(n)


  IF (b%l                   .NE. 4)   ERROR STOP 11
  IF (UBOUND(b%arr, 1)      .NE. 4)   ERROR STOP 12
  IF (SIZE(b%arr)           .NE. 4)   ERROR STOP 13
  IF (UBOUND(b%arr(1)%i, 1) .NE. 4)   ERROR STOP 14
  IF (SIZE(b%arr(1)%i)      .NE. 4)   ERROR STOP 15
  IF (ANY(b%arr(1)%i        .NE. 1))  ERROR STOP 16
  IF (ANY(b%arr(1)%j        .NE.-1))  ERROR STOP 17


  END SUBROUTINE

  END

