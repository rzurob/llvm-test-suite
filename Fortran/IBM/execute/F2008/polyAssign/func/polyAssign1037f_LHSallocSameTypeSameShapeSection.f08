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
!* DESCRIPTION
!*                              : Test section array operation for polymorphic assignment.
!*                              : Test whether the array of an intrinsic assignment is allowed to be polymorphic for the same type and different bounds.
!*                              : We test array polymorphic assignment to two levels of extensible derived types with integer type inside.
!*                              : Test the situations that all arrays on the LHS and RHS are allocated, assign with the same type and the same shape, but different bounds, i.e. same rank and extent but different bounds.
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

Program polyAssign1037f
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

    b1(2:5)=b2(2:5)
    if (lbound(b1,1) /= 2 ) error stop 31
    if (ubound(b1,1) /= 11 ) error stop 32
    if (b1(2)%i1 /= 2) error stop 33
    if (b1(3)%i1 /= 2) error stop 34
    if (b1(4)%i1 /= 2) error stop 35
    if (b1(5)%i1 /= 2) error stop 36
    if (b1(6)%i1 /= 1) error stop 37
    if (b1(7)%i1 /= 1) error stop 38
    if (b1(8)%i1 /= 1) error stop 39
    if (b1(9)%i1 /= 1) error stop 40
    if (b1(10)%i1 /= 1) error stop 41
    if (b1(11)%i1 /= 1) error stop 42

    b1=b2(2:5)

    if (lbound(b1,1) /= 1 ) error stop 43
    if (ubound(b1,1) /= 4 ) error stop 44
    if (b1(1)%i1 /= 2) error stop 45
    if (b1(2)%i1 /= 2) error stop 46
    if (b1(3)%i1 /= 2) error stop 47
    if (b1(4)%i1 /= 2) error stop 48

    b1 = b2   !deallocation, and  reset the lbound and ubound for b1.
    if (lbound(b1,1) /= 1 ) error stop 49
    if (ubound(b1,1) /= 10 ) error stop 50
    if (b1(1)%i1 /= 2) error stop 51
    if (b1(2)%i1 /= 2) error stop 60
    if (b1(3)%i1 /= 2) error stop 61
    if (b1(4)%i1 /= 2) error stop 62
    if (b1(5)%i1 /= 2) error stop 63
    if (b1(6)%i1 /= 2) error stop 64
    if (b1(7)%i1 /= 2) error stop 65
    if (b1(8)%i1 /= 2) error stop 66
    if (b1(9)%i1 /= 2) error stop 67
    if (b1(10)%i1 /= 2) error stop 68

End

