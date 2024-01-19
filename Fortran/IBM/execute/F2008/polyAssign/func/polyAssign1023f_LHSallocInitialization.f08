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
!*                              : Test the basic situation that the LHS array is allocated, and the initialization for LHS with base derived type.
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

Program polyAssign1023f
    use m
    class(base), allocatable :: b1(:)

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

End

