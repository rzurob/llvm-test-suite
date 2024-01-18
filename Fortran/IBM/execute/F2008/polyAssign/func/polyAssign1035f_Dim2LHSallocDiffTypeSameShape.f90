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
!*                              : Test when LHS is allocated initially and RHS is allocated and hve different types. LHS is base type and RHS is an extended derived type.
!*                              : --LHS and RHS have different type and the same shape.
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

Program polyAssign1035f
    use m
    integer :: i
    class(base), allocatable :: b1(:), b3(:), a1(:,:), a3(:,:)

    allocate(base :: b1(2:11))  !type base
    allocate(child :: b3(2:11)) !different types

    allocate(base :: a1(-2:-1,2:11))
    allocate(child :: a3(0:1,2:11))

    if ( allocated(b1) .eqv.  .false. ) error stop "allocated(b1) status should not be false initially."

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

   select type (b3)
      type is (child)
        if (lbound(b3,1) /= 2 ) error stop 25
        if (ubound(b3,1) /= 11 ) error stop 26
        do i = lbound(b3,1), ubound(b3,1) !2, 11
          b3(i)%i1 = i
          b3(i)%i2 = i + 10
        end do
        if (b3(2)%i1 /= 2) error stop 27
        if (b3(3)%i1 /= 3) error stop 28
        if (b3(4)%i1 /= 4) error stop 29
        if (b3(5)%i1 /= 5) error stop 30
        if (b3(6)%i1 /= 6) error stop 31
        if (b3(7)%i1 /= 7) error stop 32
        if (b3(8)%i1 /= 8) error stop 33
        if (b3(9)%i1 /= 9) error stop 34
        if (b3(10)%i1 /= 10) error stop 35
        if (b3(11)%i1 /= 11) error stop 36

        if (b3(2)%i2 /= 12) error stop 37
        if (b3(3)%i2 /= 13) error stop 38
        if (b3(4)%i2 /= 14) error stop 39
        if (b3(5)%i2 /= 15) error stop 40
        if (b3(6)%i2 /= 16) error stop 41
        if (b3(7)%i2 /= 17) error stop 42
        if (b3(8)%i2 /= 18) error stop 43
        if (b3(9)%i2 /= 19) error stop 44
        if (b3(10)%i2 /= 20) error stop 45
        if (b3(11)%i2 /= 21) error stop 46
      class default
        error stop  "type of b3 is not child!"
    end select

    b1 = b3    !! Deallocation because the dynamic type is different, b1 = b3 will make b1 get exactly the same copy of b3
    if ( allocated(b1) .eqv. .false. ) error stop " b1 = b3 will make b1 get exactly copy as b3!"
    if (lbound(b1,1) /= 2) error stop "(lbound(b1,1) should be 2 as b3."
    if (ubound(b1,1) /= 11) error stop "ubound(b1,1) should be 11 as b3."
    select type (b1)
      type is (child)
        if (b1(2)%i1 /= 2) error stop 72
        if (b1(3)%i1 /= 3) error stop 73
        if (b1(4)%i1 /= 4) error stop 74
        if (b1(5)%i1 /= 5) error stop 75
        if (b1(6)%i1 /= 6) error stop 76
        if (b1(7)%i1 /= 7) error stop 77
        if (b1(8)%i1 /= 8) error stop 78
        if (b1(9)%i1 /= 9) error stop 79
        if (b1(10)%i1 /= 10) error stop 80
        if (b1(11)%i1 /= 11) error stop 81

        if (b1(2)%i2 /= 12) error stop 82
        if (b1(3)%i2 /= 13) error stop 83
        if (b1(4)%i2 /= 14) error stop 84
        if (b1(5)%i2 /= 15) error stop 85
        if (b1(6)%i2 /= 16) error stop 86
        if (b1(7)%i2 /= 17) error stop 87
        if (b1(8)%i2 /= 18) error stop 88
        if (b1(9)%i2 /= 19) error stop 89
        if (b1(10)%i2 /= 20) error stop 90
        if (b1(11)%i2 /= 21) error stop 91
      class default
        error stop  "type of b1 is not child!"
    end select

    if ( allocated(a1) .eqv.  .false. ) error stop "allocated(a1) status should not be false initially."
    a1 = base(1)
    if ( allocated(a1) .eqv.  .false. ) error stop "allocated(a1) status should not be false after the initiallization."


    if (lbound(a1,1) /= -2) error stop "(lbound(a1,1) should be -2 now."
    if (ubound(a1,1) /= -1) error stop "ubound(a1,1) should be -1 now"
    if (lbound(a1,2) /= 2) error stop "(lbound(a1,2) should be 2 now."
    if (ubound(a1,2) /= 11) error stop "ubound(a1,2) should be 11 now"

    if (a1(-2,2)%i1 /= 1) error stop 303
    if (a1(-2,3)%i1 /= 1) error stop 304
    if (a1(-2,4)%i1 /= 1) error stop 305
    if (a1(-2,5)%i1 /= 1) error stop 306
    if (a1(-2,6)%i1 /= 1) error stop 307
    if (a1(-2,7)%i1 /= 1) error stop 308
    if (a1(-2,8)%i1 /= 1) error stop 309
    if (a1(-2,9)%i1 /= 1) error stop 310
    if (a1(-2,10)%i1 /= 1) error stop 311
    if (a1(-2,11)%i1 /= 1) error stop 312

    if (a1(-1,2)%i1 /= 1) error stop 313
    if (a1(-1,3)%i1 /= 1) error stop 314
    if (a1(-1,4)%i1 /= 1) error stop 315
    if (a1(-1,5)%i1 /= 1) error stop 316
    if (a1(-1,6)%i1 /= 1) error stop 317
    if (a1(-1,7)%i1 /= 1) error stop 318
    if (a1(-1,8)%i1 /= 1) error stop 319
    if (a1(-1,9)%i1 /= 1) error stop 320
    if (a1(-1,10)%i1 /= 1) error stop 321
    if (a1(-1,11)%i1 /= 1) error stop 322


    select type (a3)
      type is (child)
        if (lbound(a3,1) /= 0) error stop 92
        if (ubound(a3,1) /= 1) error stop 93
        if (lbound(a3,2) /= 2) error stop 94
        if (ubound(a3,2) /= 11) error stop 95
        do i = lbound(a3,2), ubound(a3,2) !2, 11
          a3(0,i)%i1 = i
          a3(0,i)%i2 = i + 10
          a3(1,i)%i1 = i
          a3(1,i)%i2 = i + 10
        end do
        if (a3(0,2)%i1 /= 2) error stop 97
        if (a3(0,3)%i1 /= 3) error stop 98
        if (a3(0,4)%i1 /= 4) error stop 99
        if (a3(0,5)%i1 /= 5) error stop 100
        if (a3(0,6)%i1 /= 6) error stop 101
        if (a3(0,7)%i1 /= 7) error stop 102
        if (a3(0,8)%i1 /= 8) error stop 103
        if (a3(0,9)%i1 /= 9) error stop 104
        if (a3(0,10)%i1 /= 10) error stop 105
        if (a3(0,11)%i1 /= 11) error stop 106

        if (a3(1,2)%i2 /= 12) error stop 107
        if (a3(1,3)%i2 /= 13) error stop 108
        if (a3(1,4)%i2 /= 14) error stop 109
        if (a3(1,5)%i2 /= 15) error stop 110
        if (a3(1,6)%i2 /= 16) error stop 111
        if (a3(1,7)%i2 /= 17) error stop 112
        if (a3(1,8)%i2 /= 18) error stop 113
        if (a3(1,9)%i2 /= 19) error stop 114
        if (a3(1,10)%i2 /= 20) error stop 115
        if (a3(1,11)%i2 /= 21) error stop 116
      class default
        error stop  "type of a3 is not child!"
    end select

    a1 = a3    !! Deallocation because the type and shape are different
    select type (a1)
      type is (child)
        if (lbound(a1,1) /= 0) error stop 122
        if (ubound(a1,1) /= 1) error stop 123
        if (lbound(a1,2) /= 2) error stop 124
        if (ubound(a1,2) /= 11) error stop 125
        if (a1(0,2)%i1 /= 2) error stop 127
        if (a1(0,3)%i1 /= 3) error stop 128
        if (a1(0,4)%i1 /= 4) error stop 129
        if (a1(0,5)%i1 /= 5) error stop 130
        if (a1(0,6)%i1 /= 6) error stop 131
        if (a1(0,7)%i1 /= 7) error stop 132
        if (a1(0,8)%i1 /= 8) error stop 133
        if (a1(0,9)%i1 /= 9) error stop 134
        if (a1(0,10)%i1 /= 10) error stop 135
        if (a1(0,11)%i1 /= 11) error stop 136

        if (a1(1,2)%i2 /= 12) error stop 137
        if (a1(1,3)%i2 /= 13) error stop 138
        if (a1(1,4)%i2 /= 14) error stop 139
        if (a1(1,5)%i2 /= 15) error stop 140
        if (a1(1,6)%i2 /= 16) error stop 141
        if (a1(1,7)%i2 /= 17) error stop 142
        if (a1(1,8)%i2 /= 18) error stop 143
        if (a1(1,9)%i2 /= 19) error stop 144
        if (a1(1,10)%i2 /= 20) error stop 145
        if (a1(1,11)%i2 /= 21) error stop 146
      class default
        error stop  "type of a3 is not child after a1 = a3!"
    end select

End

