! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : F2008/polyAssign/func/polyAssign1025f_LHSallocSameTypeDiffShape.f
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
!*                              : Test the situations that all arrays on the LHS and RHS are allocated, but LHS and RHS have different shapes, i.e. same rank but different extents.
!*                              :   -In the above situation LHS should get exactly the same copy as RHS, and the extent is changed after polymorphic assignment.
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

Program polyAssign1025f
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
    if (lbound(b4,1) /= 2 ) error stop 47
    if (ubound(b4,1) /= 12 ) error stop 48
    !print *, 'after b4 = base(4)'
    if (b4(2)%i1 /= 4) error stop 49
    if (b4(3)%i1 /= 4) error stop 50
    if (b4(4)%i1 /= 4) error stop 51
    if (b4(5)%i1 /= 4) error stop 52
    if (b4(6)%i1 /= 4) error stop 53
    if (b4(7)%i1 /= 4) error stop 54
    if (b4(8)%i1 /= 4) error stop 55
    if (b4(9)%i1 /= 4) error stop 56
    if (b4(10)%i1 /= 4) error stop 57
    if (b4(11)%i1 /= 4) error stop 58
    if (b4(12)%i1 /= 4) error stop 59

    b1 = b4    !! Deallocation because of the shape is different
    if (lbound(b1,1) /= 2 ) error stop 111
    if (ubound(b1,1) /= 12 ) error stop 112
    if (b1(2)%i1 /= 4) error stop 113
    if (b1(3)%i1 /= 4) error stop 114
    if (b1(4)%i1 /= 4) error stop 115
    if (b1(5)%i1 /= 4) error stop 116
    if (b1(6)%i1 /= 4) error stop 117
    if (b1(7)%i1 /= 4) error stop 118
    if (b1(8)%i1 /= 4) error stop 119
    if (b1(9)%i1 /= 4) error stop 110
    if (b1(10)%i1 /= 4) error stop 111
    if (b1(11)%i1 /= 4) error stop 112
    if (b1(12)%i1 /= 4) error stop 113

End

