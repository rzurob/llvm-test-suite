! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : F2008/polyAssign/func/polyAssign1039f_LHSallocDiffTypeDiffShapeSection.f
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
!*                              : Test section array operation for polymorphic assignment for array with different type and different shape.
!*                              : LHS and RHS are different type: LHS is base type and RHS is an extended derived type.
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

Program polyAssign1039f
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

    b1(2:10:2)=b5(-2:2) !b1 is still base, and only i1 part is copied to b1 
    select type (b1)
      type is (base)
        if (lbound(b1,1) /= 2 ) error stop 51
        if (ubound(b1,1) /= 11 ) error stop 52
        if (b1(2)%i1 /= 2) error stop 53
        if (b1(3)%i1 /= 1) error stop 54
        if (b1(4)%i1 /= 3) error stop 55
        if (b1(5)%i1 /= 1) error stop 56
        if (b1(6)%i1 /= 4) error stop 57
        if (b1(7)%i1 /= 1) error stop 58
        if (b1(8)%i1 /= 5) error stop 59
        if (b1(9)%i1 /= 1) error stop 60
        if (b1(10)%i1 /= 6) error stop 61
        if (b1(11)%i1 /= 1) error stop 62
      type is (child)
        error stop 63
      class default
        error stop 64
    end select




    b1 = b5(-2:1) !b1 is changed to type of child
    if ( allocated(b1) .eqv. .false. ) error stop 65 
    if (lbound(b1,1) /= 1) error stop 66
    if (ubound(b1,1) /= 4) error stop 67
    select type (b1)
      type is (child)
        if (b1(1)%i1 /= 2) error stop 78
        if (b1(2)%i1 /= 3) error stop 78
        if (b1(3)%i1 /= 4) error stop 79
        if (b1(4)%i1 /= 5) error stop 80

        if (b1(1)%i2 /= 12) error stop 87
        if (b1(2)%i2 /= 13) error stop 88
        if (b1(3)%i2 /= 14) error stop 89
        if (b1(4)%i2 /= 15) error stop 90
      class default
        error stop  91
    end select

End

