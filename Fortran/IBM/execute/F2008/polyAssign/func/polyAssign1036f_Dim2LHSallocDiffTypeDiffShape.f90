! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : F2008/polyAssign/func/polyAssign1036f_Dim2LHSallocDiffTypeDiffShape.f
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
!*                              : Test when LHS is allocated initially and RHS is allocated and have different types. LHS is base type and RHS is an extended derived type.
!*                              : --LHS and RHS have different type and different shape.
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

Program polyAssign1036f
    use m
    integer :: i
    class(base), allocatable :: b1(:), b5(:), a1(:,:), a5(:,:)

    allocate(base :: b1(2:11))  !type base  !<--Test when LHS is unallocated initially.
    allocate(child :: b5(-2:2)) !different types, different bounds

    allocate(base :: a1(-2:-1,2:11))
    allocate(child :: a5(2:3,-2:2))

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


    if ( allocated(a1) .eqv.  .false. ) error stop "allocated(a1) status should not be false initially."
    a1 = base(1) !<--LHS is unallocated initially, the assignment will not be effectiv
    if ( allocated(a1) .eqv.  .false. ) error stop "allocated(a1) status should be not be false after the initiallization."


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

    select type (a5)
      type is (child)
        if (lbound(a5,1) /= 2 ) error stop 142
        if (ubound(a5,1) /= 3 ) error stop 143
        if (lbound(a5,2) /= -2 ) error stop 144
        if (ubound(a5,2) /= 2 ) error stop 146
        do i = lbound(a5,2), ubound(a5,2) !-2, 2
          a5(2,i)%i1 = i + 4
          a5(2,i)%i2 = i + 14
          a5(3,i)%i1 = i + 4
          a5(3,i)%i2 = i + 14
        end do
        if (a5(2,-2)%i1 /= 2) error stop 147
        if (a5(2,-1)%i1 /= 3) error stop 148
        if (a5(2,0)%i1 /= 4) error stop 149
        if (a5(2,1)%i1 /= 5) error stop 150
        if (a5(2,2)%i1 /= 6) error stop 151

        if (a5(3,-2)%i2 /= 12) error stop 157
        if (a5(3,-1)%i2 /= 13) error stop 158
        if (a5(3,0)%i2 /= 14) error stop 159
        if (a5(3,1)%i2 /= 15) error stop 160
        if (a5(3,2)%i2 /= 16) error stop 161
      class default
        error stop  "type of a5 is not child!"
    end select

    a1 = a5    !! Deallocation because the type and shape are different

    select type (a1)
      type is (child)
        if (lbound(a1,1) /= 2 ) error stop 162
        if (ubound(a1,1) /= 3 ) error stop 163
        if (lbound(a1,2) /= -2 ) error stop 164
        if (ubound(a1,2) /= 2 ) error stop 166
        do i = lbound(a5,2), ubound(a5,2) !-2, 2
          a1(2,i)%i1 = i + 4
          a1(2,i)%i2 = i + 14
          a1(3,i)%i1 = i + 4
          a1(3,i)%i2 = i + 14
        end do
        if (a1(2,-2)%i1 /= 2) error stop 167
        if (a1(2,-1)%i1 /= 3) error stop 168
        if (a1(2,0)%i1 /= 4) error stop 169
        if (a1(2,1)%i1 /= 5) error stop 170
        if (a1(2,2)%i1 /= 6) error stop 171

        if (a1(3,-2)%i2 /= 12) error stop 177
        if (a1(3,-1)%i2 /= 13) error stop 178
        if (a1(3,0)%i2 /= 14) error stop 179
        if (a1(3,1)%i2 /= 15) error stop 180
        if (a1(3,2)%i2 /= 16) error stop 181
      class default
        error stop  "type of a5 is not child after a1 = a5!"
    end select

End

