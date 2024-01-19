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
!*                              : Test section array operation for polymorphic assignment for arrays with the same type and different shape.
!*                              : Test the situations that all arrays on the LHS and RHS are allocated, but LHS and RHS have different shapes, i.e. same rank but different extents.
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

Program polyAssign1038f
    use m
    class(base), allocatable :: b1(:), b4(:) !same derived type

    allocate(base :: b1(2:11))  !type base
    allocate(base :: b4(2:12))  !different bounds and extents

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

    b4 = base(4)
    if (lbound(b4,1) /= 2 ) error stop 17
    if (ubound(b4,1) /= 12 ) error stop 18
    if (b4(2)%i1 /= 4) error stop 19
    if (b4(3)%i1 /= 4) error stop 20
    if (b4(4)%i1 /= 4) error stop 21
    if (b4(5)%i1 /= 4) error stop 22
    if (b4(6)%i1 /= 4) error stop 23
    if (b4(7)%i1 /= 4) error stop 24
    if (b4(8)%i1 /= 4) error stop 25
    if (b4(9)%i1 /= 4) error stop 26
    if (b4(10)%i1 /= 4) error stop 27
    if (b4(11)%i1 /= 4) error stop 28
    if (b4(12)%i1 /= 4) error stop 29

    b1(2:10:2)=b4(2:6)
    if (lbound(b1,1) /= 2 ) error stop 31
    if (ubound(b1,1) /= 11 ) error stop 32
    if (b1(2)%i1 /= 4) error stop 33
    if (b1(3)%i1 /= 1) error stop 34
    if (b1(4)%i1 /= 4) error stop 35
    if (b1(5)%i1 /= 1) error stop 36
    if (b1(6)%i1 /= 4) error stop 37
    if (b1(7)%i1 /= 1) error stop 38
    if (b1(8)%i1 /= 4) error stop 39
    if (b1(9)%i1 /= 1) error stop 40
    if (b1(10)%i1 /= 4) error stop 41
    if (b1(11)%i1 /= 1) error stop 42

    b1 = b4 (3:8)
    if (lbound(b1,1) /= 1 ) error stop 50
    if (ubound(b1,1) /= 6 ) error stop 51
    if (b1(1)%i1 /= 4) error stop 52
    if (b1(2)%i1 /= 4) error stop 53
    if (b1(3)%i1 /= 4) error stop 54
    if (b1(4)%i1 /= 4) error stop 55
    if (b1(5)%i1 /= 4) error stop 56
    if (b1(6)%i1 /= 4) error stop 57

End

