! *********************************************************************
!* ===================================================================
!*
!* FEATURE                      : F2008: LHS of intrinsic assignment is allowed to be polymorphic (96086)
!*                                https://compjazz.torolab.ibm.com:9443/jazz/resource/itemName/com.ibm.team.workitem.WorkItem/96086
!* DATE                         : 07 August 2015
!*
!* PRIMARY FUNCTIONS TESTED     : F2008: LHS of intrinsic assignment is allowed to be polymorphic
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : disabled from running list
!*                              : Test the situations that LHS is allocated, RHS is unallocated.
!*                              : Test whether the array of an intrinsic assignment is allowed to be polymorphic for different dynamic type, extents, and bounds.
!*                              : We test array polymorphic assignment to two levels of extensible derived types with integer type inside.
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  08/20/15    AL     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base
      integer :: i1
   end type
   type, extends(base) :: child
      integer :: i2
   end type
end module

Program polyAssign1028
    use m
    class(base), allocatable :: b1(:), b2(:) !same derived type

    allocate(base :: b1(2:11))  !type base

    b1 = base(1)
    if (lbound(b1,1) /= 2 ) error stop 1
    if (ubound(b1,1) /= 11 ) error stop 2
    if (b1(2)%i1 /= 1) error stop 3
    if (b1(3)%i1 /= 1) error stop 4
    if (b1(4)%i1 /= 1) error stop 5
    if (b1(5)%i1 /= 1) error stop 6
    if (b1(6)%i1 /= 1) error stop 7
    if (b1(7)%i1 /= 1) error stop 8
    if (b1(8)%i1 /= 1) error stop 9
    if (b1(9)%i1 /= 1) error stop 10
    if (b1(10)%i1 /= 1) error stop 11
    if (b1(11)%i1 /= 1) error stop 12

    if ( allocated(b1) .eqv.  .false. ) error stop 13
    if ( allocated(b2) .neqv.  .false. ) error stop 14
    !if ( shape(b1) /= 10 ) error stop 15
    !if ( shape(b2) /= 0) error stop 16
    if ( size(b1) /= 10 ) error stop 17
    if ( size(b2) /= 0 ) error stop 18

    if (lbound(b2,1) /= 1 ) error stop 19
    if (ubound(b2,1) /= 0 ) error stop 20

    b1 = b2   !deallocation and reallocation, reset the bounds for b1.


    if ( allocated(b1) .eqv.  .false. ) error stop 21
    if ( allocated(b2) .neqv.  .false. ) error stop 22
    !if ( shape(b1) /= 0 ) error stop 23
    !if ( shape(b2) /= 0) error stop 24
    if ( size(b1) /= 0 ) error stop 25
    if ( size(b2) /= 0 ) error stop 26

    if (lbound(b2,1) /= 1 ) error stop 27
    if (ubound(b2,1) /= 0 ) error stop 28
    if (lbound(b1,1) /= 1 ) error stop 29
    if (ubound(b1,1) /= 0 ) error stop 30

End

