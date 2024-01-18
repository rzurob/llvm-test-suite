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
!*                              : Test the situations that all arrays on the LHS and RHS are allocated, assign with the same type and the same shape, but different bounds, i.e. same rank and extent but different bounds.
!*                              :    -In the above situation LHS should have element by element copy from RHS, but the bounds should not be changed.
!*                              : We add arrays with rank=2, LHS is allocated, RHS is allocated.
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

Program polyAssign1033f
    use m
    class(base), allocatable :: b1(:), b2(:), a1(:,:), a2(:,:) !same derived type

    allocate(base :: b1(2:11))  !type base
    allocate(base:: b2(1:10))   !different bounds and the same extent

    allocate(base :: a1(-2:-1,2:11))
    allocate(base :: a2(-1:0,1:10))

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
    !print *, 'Should have no deallocation, and should not reset the lbound for b1.'
    if (lbound(b1,1) /= 2 ) error stop 101
    if (ubound(b1,1) /= 11) error stop 102
    if (b1(2)%i1 /= 2) error stop 60
    if (b1(3)%i1 /= 2) error stop 61
    if (b1(4)%i1 /= 2) error stop 62
    if (b1(5)%i1 /= 2) error stop 63
    if (b1(6)%i1 /= 2) error stop 64
    if (b1(7)%i1 /= 2) error stop 65
    if (b1(8)%i1 /= 2) error stop 66
    if (b1(9)%i1 /= 2) error stop 67
    if (b1(10)%i1 /= 2) error stop 68
    if (b1(11)%i1 /= 2) error stop 59

    if ( allocated(a1) .eqv.  .false. ) error stop "allocated(a1) status should not be false initially."
    a1 = base(1) !<--LHS is allocated initially
    if ( allocated(a1) .eqv.  .false. ) error stop "allocated(a1) status should not be false after the initiallization."

    if (lbound(a1,1) /= -2) error stop "(lbound(a1,1) should be -2 now."
    if (ubound(a1,1) /= -1) error stop "ubound(a1,1) should be -1 now"
    if (lbound(a1,2) /= 2) error stop "(lbound(a1,2) should be 2 now."
    if (ubound(a1,2) /= 11) error stop "ubound(a1,2) should be 11 now"

    if (a1(-2,2)%i1 /= 1) error stop 203
    if (a1(-2,3)%i1 /= 1) error stop 204
    if (a1(-2,4)%i1 /= 1) error stop 205
    if (a1(-2,5)%i1 /= 1) error stop 206
    if (a1(-2,6)%i1 /= 1) error stop 207
    if (a1(-2,7)%i1 /= 1) error stop 208
    if (a1(-2,8)%i1 /= 1) error stop 209
    if (a1(-2,9)%i1 /= 1) error stop 210
    if (a1(-2,10)%i1 /= 1) error stop 211
    if (a1(-2,11)%i1 /= 1) error stop 212

    if (a1(-1,2)%i1 /= 1) error stop 213
    if (a1(-1,3)%i1 /= 1) error stop 214
    if (a1(-1,4)%i1 /= 1) error stop 215
    if (a1(-1,5)%i1 /= 1) error stop 216
    if (a1(-1,6)%i1 /= 1) error stop 217
    if (a1(-1,7)%i1 /= 1) error stop 218
    if (a1(-1,8)%i1 /= 1) error stop 219
    if (a1(-1,9)%i1 /= 1) error stop 220
    if (a1(-1,10)%i1 /= 1) error stop 221
    if (a1(-1,11)%i1 /= 1) error stop 222

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

    if (a2(0,1)%i1 /= 2) error stop 145
    if (a2(0,2)%i1 /= 2) error stop 146
    if (a2(0,3)%i1 /= 2) error stop 147
    if (a2(0,4)%i1 /= 2) error stop 148
    if (a2(0,5)%i1 /= 2) error stop 149
    if (a2(0,7)%i1 /= 2) error stop 150
    if (a2(0,8)%i1 /= 2) error stop 151
    if (a2(0,9)%i1 /= 2) error stop 152
    if (a2(0,10)%i1 /= 2) error stop 153

    a1 = a2   !<--This will make a1 get exactly copy as a2, but bounds will not be changed!
    if (lbound(a1,1) /= -2) error stop "(lbound(a1,1) should still be -2 now."
    if (ubound(a1,1) /= -1) error stop "ubound(a1,1) should still be -1 now"
    if (lbound(a1,2) /= 2) error stop "(lbound(a1,2) should still be 2 now."
    if (ubound(a1,2) /= 11) error stop "ubound(a1,2) should still  be 11 now"

    if (a1(-2,2)%i1 /= 2) error stop 303
    if (a1(-2,3)%i1 /= 2) error stop 304
    if (a1(-2,4)%i1 /= 2) error stop 305
    if (a1(-2,5)%i1 /= 2) error stop 306
    if (a1(-2,6)%i1 /= 2) error stop 307
    if (a1(-2,7)%i1 /= 2) error stop 308
    if (a1(-2,8)%i1 /= 2) error stop 309
    if (a1(-2,9)%i1 /= 2) error stop 310
    if (a1(-2,10)%i1 /= 2) error stop 311
    if (a1(-2,11)%i1 /= 2) error stop 312

    if (a1(-1,2)%i1 /= 2) error stop 313
    if (a1(-1,3)%i1 /= 2) error stop 314
    if (a1(-1,4)%i1 /= 2) error stop 315
    if (a1(-1,5)%i1 /= 2) error stop 316
    if (a1(-1,6)%i1 /= 2) error stop 317
    if (a1(-1,7)%i1 /= 2) error stop 318
    if (a1(-1,8)%i1 /= 2) error stop 319
    if (a1(-1,9)%i1 /= 2) error stop 320
    if (a1(-1,10)%i1 /= 2) error stop 321
    if (a1(-1,11)%i1 /= 2) error stop 322

End

