!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : autoobj87
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
!*  multi levels of dt
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM autoobj03

  TYPE r(l)
    INTEGER, LEN :: l
    integer :: jj(l)!=-1
    integer :: ii!=1
  END TYPE

  TYPE s(l)
    INTEGER, LEN :: l
     type(r(l)) :: rr(l)
  END TYPE

  TYPE t(l)
    INTEGER, LEN :: l
    integer :: i4(l)! =-1
    TYPE(s(l)) :: ss(1:l)
  END TYPE

  CALL SUB(4)

  CONTAINS

  SUBROUTINE Sub(N)

  TYPE(t(n)) x!(n)

    x%i4 = -1
    x%ss(1)%rr%ii = 1
    x%ss(1)%rr(1)%jj = -1

    print *, x%ss(1)%rr%ii
    print *, x%ss(1)%rr(1)%jj
    IF (x%l              .NE. 4)   STOP 11
    IF (UBOUND(x%ss, 1)  .NE. 4)   STOP 12
    IF (SIZE(x%ss)       .NE. 4)   STOP 13

    IF (x%ss%l                 .NE. 4)   STOP 21
    IF (UBOUND(x%ss(1)%rr, 1)  .NE. 4)   STOP 22
    IF (SIZE(x%ss(1)%rr)       .NE. 4)   STOP 23

    IF (x%ss(1)%rr%l                 .NE. 4)   STOP 31
    IF (UBOUND(x%ss(1)%rr(1)%jj, 1)  .NE. 4)   STOP 32
    IF (SIZE(x%ss(1)%rr(1)%jj)       .NE. 4)   STOP 33

    IF ( ANY(x%ss(1)%rr%ii       .NE. 1))   STOP 41
    IF ( ANY(x%ss(1)%rr(1)%jj    .NE. -1))  STOP 42

  END SUBROUTINE

  END
