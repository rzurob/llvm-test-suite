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
!* DESCRIPTION                  : Test whether the array of an intrinsic assignment is allowed to be polymorphic for different dynamic type, extents, and bounds.
!*                              : We test array polymorphic assignment to two levels of extensible derived types with integer type inside.
!*                              : Test when LHS is unallocated initially.
!*                              : Test the initialization for an unallocated allocatable aray with base type
!*                              : Test polymorphic assignment to an unallocated array, in this stuation the unallocated LHS is allocated with RHS, when RHS is allocated and has the same type and same shape!
!*                              : Disabled.
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

Program polyAssign1019f
    use m
    class(base), allocatable :: b1(:), b2(:)
    allocate(base:: b2(1:10))   !different bounds and the same extent
    if ( allocated(b1) .neqv.  .false. ) error stop "allocated(b1) status should be false initially."
    !b1 = base(1) !<--LHS is unallocated initially, the assignment will make b1 allocated
    !if ( allocated(b1) .eqv.  .false. ) error stop "allocated(b1) status should not be false after the initiallization."
    !if (lbound(b1,1) /= 1) error stop "(lbound(b1,1) should be 1 by default  even it is unallocated now."
    !if (ubound(b1,1) /= 0) error stop "ubound(b1,1) should be 0 to indicate a zero-sized array"

    b2 = base(2)
    if (lbound(b2,1) /= 1 ) error stop 13
    if (ubound(b2,1) /= 10 ) error stop 14
    if (b2(1)%i1 /= 2) error stop 15
    if (b2(2)%i1 /= 2) error stop 16
    if (b2(3)%i1 /= 2) error stop 17
    if (b2(4)%i1 /= 2) error stop 18
    if (b2(5)%i1 /= 2) error stop 19
    if (b2(7)%i1 /= 2) error stop 21
    if (b2(8)%i1 /= 2) error stop 22
    if (b2(9)%i1 /= 2) error stop 23
    if (b2(10)%i1 /= 2) error stop 24

    b1 = b2   !<--This will make b1 allocated, get exactly copy as b2!
    if ( allocated(b1) .eqv. .false. ) error stop " b1 = b2 will make b1 allocated, get exactly copy as b2!"
    if (lbound(b1,1) /= 1) error stop "(lbound(b1,1) should be 1 as b2."
    if (ubound(b1,1) /= 10) error stop "ubound(b1,1) should be 10 as b2"

    if (lbound(b1,1) /= 1 ) error stop 101
    if (ubound(b1,1) /= 10 ) error stop 102
    if (b1(2)%i1 /= 2) error stop 60
    if (b1(3)%i1 /= 2) error stop 61
    if (b1(4)%i1 /= 2) error stop 62
    if (b1(5)%i1 /= 2) error stop 63
    if (b1(6)%i1 /= 2) error stop 64
    if (b1(7)%i1 /= 2) error stop 65
    if (b1(8)%i1 /= 2) error stop 66
    if (b1(9)%i1 /= 2) error stop 67
    if (b1(10)%i1 /= 2) error stop 68
    if (b1(1)%i1 /= 2) error stop 69

End

