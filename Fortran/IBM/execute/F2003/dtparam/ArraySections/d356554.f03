!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sep 23, 2008
!*  PRIMARY FUNCTIONS TESTED   : See Description below.
!*  REFERENCE                  : Defect 356544 for feature 353925
!*
!234567890123456789012345678901234567890123456789012345678901234567890

type base
    integer :: x
end type

type, extends(base) :: child(n)
    integer, len :: n
    integer :: y(n)              ! replace n by 2 makes test pass
end type

type(child(2)) :: carr(2)

carr(1:2)%y(1) = [1,2]
print *,carr(1:2)%y(1)           ! ICEs here
end