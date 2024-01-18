! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : F2008/polyAssign/func/polyAssign1024f_LHSallocSameTypeSameShape.f
!*
!* FEATURE                      : F2008: LHS of intrinsic assignment is allowed to be polymorphic (96086)
!*                                https://compjazz.torolab.ibm.com:9443/jazz/resource/itemName/com.ibm.team.workitem.WorkItem/96086
!* PROGRAMMER                   : Aaron Liu
!* DATE                         : 07 August 2015
!* ORIGIN                       : IBM XL Compiler Development, IBM Software Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : F2008: LHS of intrinsic assignment is allowed to be polymorphic
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  
!*                              : Test the situations that all arrays on the LHS and RHS are allocated, assign with the same type and the same shape, but different bounds, i.e. same rank and extent but different bounds.
!*                              :    -In the above situation LHS should have element by element copy from RHS, but the bounds should not be changed.
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

Program polyAssign1024f
    use m
    class(base), allocatable :: b1(:), b2(:) !same derived type

    allocate(base :: b1(2:11))  !type base
    allocate(base:: b2(1:10))   !different bounds and the same extent

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

    b1 = b2   !no deallocation, and not reset the lbound for b1. 
    !print *, 'After intrinsic assignment "b1 = b2"'
    !print *, 'Should have no deallocation, and should not reset the lbound for b1.'
    if (lbound(b1,1) /= 2 ) error stop 101
    if (ubound(b1,1) /= 11 ) error stop 102
    if (b1(2)%i1 /= 2) error stop 60
    if (b1(3)%i1 /= 2) error stop 61
    if (b1(4)%i1 /= 2) error stop 62
    if (b1(5)%i1 /= 2) error stop 63
    if (b1(6)%i1 /= 2) error stop 64
    if (b1(7)%i1 /= 2) error stop 65
    if (b1(8)%i1 /= 2) error stop 66
    if (b1(9)%i1 /= 2) error stop 67
    if (b1(10)%i1 /= 2) error stop 68
    if (b1(11)%i1 /= 2) error stop 69

End

