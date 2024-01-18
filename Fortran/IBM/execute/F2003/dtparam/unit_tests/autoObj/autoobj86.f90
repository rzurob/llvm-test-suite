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
!*  Autoobj could be ptr or alloc
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM autoobj86

  TYPE base
    integer :: i=1
  END TYPE

  TYPE dt(l)
    INTEGER, LEN :: l
    TYPE(base) :: arr1(l)
    TYPE(base) :: arr(l)
  END TYPE

  CALL sub(4)

  CONTAINS

  SUBROUTINE Sub(N)

  TYPE(dt(n)), target ::  a!(n)
  TYPE(dt(n)), pointer :: b!(n)
  TYPE(dt(n)), allocatable :: c!(n)

  b => a
  ALLOCATE( c, source=a)

  IF (b%l               .NE. 4)   STOP 11
  IF (UBOUND(b%arr, 1)  .NE. 4)   STOP 12
  IF (UBOUND(b%arr1, 1) .NE. 4)   STOP 13

  IF (c%l               .NE. 4)   STOP 21
  IF (UBOUND(c%arr, 1)  .NE. 4)   STOP 22
  IF (UBOUND(c%arr1, 1) .NE. 4)   STOP 23

  IF ( ANY ( A%Arr%I .NE. 1 ) ) STOP 31
  IF ( ANY ( B%Arr%I .NE. 1 ) ) STOP 32
  IF ( ANY ( C%Arr%I .NE. 1 ) ) STOP 33

  END SUBROUTINE

  END

