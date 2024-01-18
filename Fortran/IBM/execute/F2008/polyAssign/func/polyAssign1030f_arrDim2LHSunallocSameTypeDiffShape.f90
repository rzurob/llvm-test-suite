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
!*                              : Test polymorphic assignment LHS is unallocated, RHS is allocated, LHS and have the same type but different shape.
!*                              : We add arrays with rank=2, LHS is unallocated, RHS is allocated.
!*                              : Test whether the array of an intrinsic assignment is allowed to be polymorphic for different dynamic type, extents, and bounds.
!*                              : We test array polymorphic assignment to two levels of extensible derived types with integer type inside.
!*                              : Test when LHS is unallocated initially.
!*                              : Test the initialization for an unallocated allocatable aray with base type
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

Program polyAssign1030f
    use m
    class(base), allocatable :: b1(:), b4(:), a1(:,:), a4(:,:)
    allocate(base :: b4(2:12))  !different bounds and extents
    allocate(base :: a4(1:2,2:12))
    if ( allocated(b1) .neqv.  .false. ) error stop "allocated(b1) status should be false initially."
    !b1 = base(1)
    !if ( allocated(b1) .eqv.  .false. ) error stop "allocated(b1) status should not be false after the initiallization."
    !if (lbound(b1,1) /= 1) error stop "(lbound(b1,1) should be 1 by default"
    !if (ubound(b1,1) /= 0) error stop "ubound(b1,1) should be 0 to indicate a zero-sized array"
    b4 = base(4)
    if (lbound(b4,1) /= 2 ) error stop 47
    if (ubound(b4,1) /= 12 ) error stop 48
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
    if (lbound(b1,1) /= 2 ) error stop 61
    if (ubound(b1,1) /= 12 ) error stop 62
    if (b1(2)%i1 /= 4) error stop 63
    if (b1(3)%i1 /= 4) error stop 64
    if (b1(4)%i1 /= 4) error stop 65
    if (b1(5)%i1 /= 4) error stop 66
    if (b1(6)%i1 /= 4) error stop 67
    if (b1(7)%i1 /= 4) error stop 68
    if (b1(8)%i1 /= 4) error stop 69
    if (b1(9)%i1 /= 4) error stop 70
    if (b1(10)%i1 /= 4) error stop 71
    if (b1(11)%i1 /= 4) error stop 72
    if (b1(12)%i1 /= 4) error stop 73

    if ( allocated(a1) .neqv.  .false. ) error stop "allocated(a1) status should be false initially."
    !a1 = base(1)
    !if ( allocated(a1) .eqv. .false. ) error stop "allocated(a1) status should not be false after the initiallization."
    !if (lbound(a1,1) /= 1) error stop "(lbound(a1,1) should be 1 by default."
    !if (ubound(a1,1) /= 0) error stop "ubound(a1,1) should be 0 to indicate a zero-sized array."
    !if (lbound(a1,2) /= 1) error stop "(lbound(a1,2) should be 1 by default."
    !if (ubound(a1,2) /= 0) error stop "ubound(a1,2) should be 0 to indicate a zero-sized array."

    a4 = base(4)
    if (lbound(a4,1) /= 1) error stop 79
    if (ubound(a4,1) /= 2) error stop 80
    if (lbound(a4,2) /= 2) error stop 81
    if (ubound(a4,2) /= 12) error stop 82
    if (a4(1,2)%i1 /= 4) error stop 83
    if (a4(1,3)%i1 /= 4) error stop 84
    if (a4(1,4)%i1 /= 4) error stop 85
    if (a4(1,5)%i1 /= 4) error stop 86
    if (a4(1,6)%i1 /= 4) error stop 87
    if (a4(1,7)%i1 /= 4) error stop 88
    if (a4(1,8)%i1 /= 4) error stop 89
    if (a4(1,9)%i1 /= 4) error stop 90
    if (a4(1,10)%i1 /= 4) error stop 91
    if (a4(1,11)%i1 /= 4) error stop 92
    if (a4(2,12)%i1 /= 4) error stop 93
    if (a4(2,2)%i1 /= 4) error stop 931
    if (a4(2,3)%i1 /= 4) error stop 94
    if (a4(2,4)%i1 /= 4) error stop 95
    if (a4(2,5)%i1 /= 4) error stop 96
    if (a4(2,6)%i1 /= 4) error stop 97
    if (a4(2,7)%i1 /= 4) error stop 98
    if (a4(2,8)%i1 /= 4) error stop 99
    if (a4(2,9)%i1 /= 4) error stop 100
    if (a4(2,10)%i1 /= 4) error stop 101
    if (a4(2,11)%i1 /= 4) error stop 102
    if (a4(2,12)%i1 /= 4) error stop 103

    a1 = a4    !! Deallocation because the shape is different
    if (lbound(a1,1) /= 1) error stop 109
    if (ubound(a1,1) /= 2) error stop 110
    if (lbound(a1,2) /= 2) error stop 111
    if (ubound(a1,2) /= 12) error stop 112
    if (a1(1,2)%i1 /= 4) error stop 113
    if (a1(1,3)%i1 /= 4) error stop 114
    if (a1(1,4)%i1 /= 4) error stop 115
    if (a1(1,5)%i1 /= 4) error stop 116
    if (a1(1,6)%i1 /= 4) error stop 117
    if (a1(1,7)%i1 /= 4) error stop 118
    if (a1(1,8)%i1 /= 4) error stop 119
    if (a1(1,9)%i1 /= 4) error stop 120
    if (a1(1,10)%i1 /= 4) error stop 121
    if (a1(1,11)%i1 /= 4) error stop 122
    if (a1(2,12)%i1 /= 4) error stop 123
    if (a1(2,2)%i1 /= 4) error stop 124
    if (a1(2,3)%i1 /= 4) error stop 134
    if (a1(2,4)%i1 /= 4) error stop 135
    if (a1(2,5)%i1 /= 4) error stop 136
    if (a1(2,6)%i1 /= 4) error stop 137
    if (a1(2,7)%i1 /= 4) error stop 138
    if (a1(2,8)%i1 /= 4) error stop 139
    if (a1(2,9)%i1 /= 4) error stop 140
    if (a1(2,10)%i1 /= 4) error stop 141
    if (a1(2,11)%i1 /= 4) error stop 142
    if (a1(2,12)%i1 /= 4) error stop 143

End

