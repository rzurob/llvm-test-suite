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
!*                              : LHS and RHS are different type: LHS is base type and RHS is an extended derived type.
!*                              : LHS and RHS have different shape.
!*                              : Test whether the array of an intrinsic assignment is allowed to be polymorphic for different dynamic type, extents, and bounds.
!*                              : We test array polymorphic assignment to two levels of extensible derived types with integer type inside.
!*                              : Test the situations that all arrays on the LHS and RHS are allocated.
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

Program polyAssign1027f
    use m
    integer :: i
    class(base), allocatable :: b1(:), b5(:)

    allocate(base :: b1(2:11))  !type base
    allocate(child :: b5(-2:2)) !different types, different bounds

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

    select type (b5)
      type is (child)
        if (lbound(b5,1) /= -2 ) error stop 25
        if (ubound(b5,1) /= 2 ) error stop 26
        do i = lbound(b5,1), ubound(b5,1) !-2, 2
          b5(i)%i1 = i + 4
          b5(i)%i2 = i + 14
        end do
        if (b5(-2)%i1 /= 2) error stop 27
        if (b5(-1)%i1 /= 3) error stop 28
        if (b5(0)%i1 /= 4) error stop 29
        if (b5(1)%i1 /= 5) error stop 30
        if (b5(2)%i1 /= 6) error stop 31

        if (b5(-2)%i2 /= 12) error stop 37
        if (b5(-1)%i2 /= 13) error stop 38
        if (b5(0)%i2 /= 14) error stop 39
        if (b5(1)%i2 /= 15) error stop 40
        if (b5(2)%i2 /= 16) error stop 41
      class default
        error stop  "type of b5 is not child!"
    end select

    b1 = b5    !! Deallocation because the dynamic type and shape are different. This will make b1 allocated and get exactly the same copy of b5
    !print *, "allocated(b1)==", allocated(b1) !<--LHS is unallocated initially
    if ( allocated(b1) .eqv. .false. ) error stop " b1 = b5 will make b1 allocated, get exactly copy as b5!"
    !print *, lbound(b1,1) !<--(lbound(b1,1))==-2 as b5
    if (lbound(b1,1) /= -2) error stop "(lbound(b1,1) should be -2 as b5."
    !print *, ubound(b1,1) !<--ubound(b1,1)==2 as b5
    if (ubound(b1,1) /= 2) error stop "ubound(b1,1) should be 2 as b5."
    !print *, 'Should have deallocation for b1 as the dynamic type of b1 and b5 are different.'
    select type (b1)
      type is (child)
        if (b1(-2)%i1 /= 2) error stop 127
        if (b1(-1)%i1 /= 3) error stop 128
        if (b1(0)%i1 /= 4) error stop 129
        if (b1(1)%i1 /= 5) error stop 130
        if (b1(2)%i1 /= 6) error stop 131

        if (b1(-2)%i2 /= 12) error stop 137
        if (b1(-1)%i2 /= 13) error stop 138
        if (b1(0)%i2 /= 14) error stop 139
        if (b1(1)%i2 /= 15) error stop 140
        if (b1(2)%i2 /= 16) error stop 141
      class default
        error stop  "type of b1 is not child!"
    end select

End

