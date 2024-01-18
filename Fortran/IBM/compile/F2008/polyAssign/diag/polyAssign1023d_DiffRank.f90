! *********************************************************************
!* ===================================================================
!*
!* FEATURE                      : F2008: LHS of intrinsic assignment is allowed to be polymorphic (96086)
!*                                https://compjazz.torolab.ibm.com:9443/jazz/resource/itemName/com.ibm.team.workitem.WorkItem/96086
!* DATE                         : 07 August 2015
!*
!* PRIMARY FUNCTIONS TESTED     : F2008: LHS of intrinsic assignment is allowed to be polymorphic
!* SECONDARY FUNTIONS TESTED    : LHS of intrinsic assignment with different ranks
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : LHS rank=1, RHS rank=2
!*                              : Test polymorphic assignment with different ranks.
!*                              : When do assignment between different ranks, will produce the "Operands must be conformable." errors
!*                              : Test whether the array of an intrinsic assignment is allowed to be polymorphic for different dynamic type, extents, and bounds.
!*                              : We test array polymorphic assignment to two levels of extensible derived types with integer type inside.
!*                              : Test when LHS is unallocated initially.
!*                              : Test polymorphic assignment to an unallocated array, in this stuation the unallocated LHS is allocated with RHS, when RHS is allocated and has the same type and same shape!
!*                              : We add arrays with rank=2, LHS is unallocated, RHS is allocated.
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

Program polyAssign1023
    use m
    class(base), allocatable :: b1(:), b2(:), a1(:,:), a2(:,:)

    allocate(base :: b2(1:10))   !different bounds and the same extent

    allocate(base :: a2(-1:0,1:10))

    if ( allocated(b1) .neqv.  .false. ) error stop "allocated(b1) status should be false initially."

    b1 = base(1) !<--LHS is unallocated initially, the assignment will not be effectiv
    if ( allocated(b1) .neqv.  .false. ) error stop "allocated(b1) status should be false after the uneffective initiallization."
    if (lbound(b1,1) /= 1) error stop "(lbound(b1,1) should be 1 by default  even it is unallocated now."
    if (ubound(b1,1) /= 0) error stop "ubound(b1,1) should be 0 to indicate a zero-sized array, since it is unallocated"

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
    if (lbound(b1,1) /= 1 ) error stop 25
    if (ubound(b1,1) /= 10 ) error stop 26
    if (b1(2)%i1 /= 2) error stop 30
    if (b1(3)%i1 /= 2) error stop 31
    if (b1(4)%i1 /= 2) error stop 32
    if (b1(5)%i1 /= 2) error stop 33
    if (b1(6)%i1 /= 2) error stop 34
    if (b1(7)%i1 /= 2) error stop 35
    if (b1(8)%i1 /= 2) error stop 36
    if (b1(9)%i1 /= 2) error stop 37
    if (b1(10)%i1 /= 2) error stop 38
    if (b1(1)%i1 /= 2) error stop 39

    if ( allocated(a1) .neqv.  .false. ) error stop "allocated(a1) status should be false initially."
    a1 = base(1) !<--LHS is unallocated initially, the assignment will not be effectiv
    if ( allocated(a1) .neqv.  .false. ) error stop "allocated(a1) status should be false after the uneffective initiallization."

    if (lbound(a1,1) /= 1) error stop "(lbound(a1,1) should be 1 by default  even it is unallocated now."
    if (ubound(a1,1) /= 0) error stop "ubound(a1,1) should be 0 to indicate a zero-sized array, since it is unallocated"
    if (lbound(a1,2) /= 1) error stop "(lbound(a1,2) should be 1 by default  even it is unallocated now."
    if (ubound(a1,2) /= 0) error stop "ubound(a1,2) should be 0 to indicate a zero-sized array, since it is unallocated"

    a2 = base(2)
    if (lbound(a2,1) /= -1) error stop 40
    if (ubound(a2,1) /= 0) error stop 41
    if (lbound(a2,2) /= 1) error stop 42
    if (ubound(a2,2) /= 10) error stop 43
    if (a2(-1,1)%i1 /= 2) error stop 45
    if (a2(-1,2)%i1 /= 2) error stop 46
    if (a2(-1,3)%i1 /= 2) error stop 47
    if (a2(-1,4)%i1 /= 2) error stop 48
    if (a2(-1,5)%i1 /= 2) error stop 49
    if (a2(-1,7)%i1 /= 2) error stop 50
    if (a2(-1,8)%i1 /= 2) error stop 51
    if (a2(-1,9)%i1 /= 2) error stop 52
    if (a2(-1,10)%i1 /= 2) error stop 53

    a1 = a2   !<--This will make a1 allocated, get exactly copy as a2!
    if ( allocated(a1) .eqv. .false. ) error stop " a1 = a2 will make a1 allocated, get exactly copy as a2!"

    if (lbound(a1,1) /= -1) error stop 60
    if (ubound(a1,1) /= 0) error stop 61
    if (lbound(a1,2) /= 1) error stop 62
    if (ubound(a1,2) /= 10) error stop 63
    if (a1(-1,1)%i1 /= 2) error stop 65
    if (a1(-1,2)%i1 /= 2) error stop 66
    if (a1(-1,3)%i1 /= 2) error stop 67
    if (a1(-1,4)%i1 /= 2) error stop 68
    if (a1(-1,5)%i1 /= 2) error stop 69
    if (a1(-1,7)%i1 /= 2) error stop 70
    if (a1(-1,8)%i1 /= 2) error stop 71
    if (a1(-1,9)%i1 /= 2) error stop 72
    if (a1(-1,10)%i1 /= 2) error stop 73

    b1 = a1 !<-- Operands must be conformable
    b2 = a2 !<-- Operands must be conformable
    b1 = a2 !<-- Operands must be conformable
    b2 = a1 !<-- Operands must be conformable
End

