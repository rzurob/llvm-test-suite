! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : F2008/polyAssign/func/polyAssign1020f_arrDim1Lv2IntLHSunallocSameTypeDiffShape.f
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
!*                              : We test array polymorphic assignment to two levels of extensible derived types with integer type inside.
!*                              : Test the initialization for an unallocated allocatable aray with base type.
!*                              : Test polymorphic assignment LHS is unallocated, RHS is allocated, LHS and have the same type but different shape.
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

Program polyAssign1020f
    use m
    class(base), allocatable :: b1(:), b4(:)
    allocate(base :: b4(2:12))  !different bounds and extents
    !print *, "allocated(b1)==", allocated(b1) !<--LHS is unallocated initially
    if ( allocated(b1) .neqv.  .false. ) error stop "allocated(b1) status should be false initially."

    !b1 = base(1) 
    !if ( allocated(b1) .eqv.  .false. ) error stop "allocated(b1) status should not be false."
    !if (lbound(b1,1) /= 1) error stop "(lbound(b1,1) should be 1 by default  even it is unallocated now."
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

    b1 = b4    
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

