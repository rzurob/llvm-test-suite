!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-10-04
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray access (specific) - assignment
!*  SECONDARY FUNCTIONS TESTED : in main program, assign pointer intrinsic variables to coarray variables and vice-versa
!*  ADAPTED FROM               : csAllocatableIntrinsic (<-csSimpleInteger<-csSimpleLogical)
!*
!*  DESCRIPTION
!*
!*  Assign simple values from pointer variables to integer coarray scalars and
!*  arrays of different kinds in the main program.  Slight change from allocatable:
!*  We also use targets.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program csPointerIntrinsic

    implicit none

    ! we're relying on the use of 2's complement arithmetic when we use an expression
    ! like "-huge(0_KIND)-1"
    integer(1), parameter :: min1 = -huge(0_1)-1,  max1 = huge(0_1), mid1 = 13_1
    integer(2), parameter :: min2 = -huge(0_2)-1,  max2 = huge(0_2), mid2 = 12345_2
    integer(4), parameter :: min4 = -huge(0_4)-1,  max4 = huge(0_4), mid4 = 898973451_4
    integer(8), parameter :: min8 = -huge(0_8)-1,  max8 = huge(0_8), mid8 = -12345678901234_8

    integer(1) :: v1, itmp1, iatmp1(:), iatgt1(10)
    integer(2) :: v2, itmp2, iatmp2(:), iatgt2(10)
    integer(4) :: v4, itmp4, iatmp4(:), iatgt4(10)
    integer(8) :: v8, itmp8, iatmp8(:), iatgt8(10)

    pointer :: v1, iatmp1, v2, iatmp2, v4, iatmp4, v8, iatmp8
    target  :: itmp1, itmp2, itmp4, itmp8, iatgt1, iatgt2, iatgt4, iatgt8

    integer(1), save :: i1[*] = 0, ia1(10)[*] = 0
    integer(2), save :: i2[*] = 0, ia2(10)[*] = 0
    integer(4), save :: i4[*] = 0, ia4(10)[*] = 0
    integer(8), save :: i8[*] = 0, ia8(10)[*] = 0

    allocate(v1, source = min1)
    allocate(v2, source = min2)
    allocate(v4, source = min4)
    allocate(v8, source = min8)

    allocate(iatmp1(10), source = [integer(1):: 1,1,2,3,5,8,13,21,34,v1])
    allocate(iatmp2(10), source = [integer(2):: v2,0,v2,0,v2,0,v2,0,v2,0])
    allocate(iatmp4(10), source = [integer(4):: 0,v4,0,v4,0,v4,0,v4,0,v4])
    allocate(iatmp8(10), source = [integer(8):: v8,0,v8,0,v8,0,v8,0,v8,0])

    ! start with the minimum value for each kind:
    i1 = v1
    i2 = v2
    i4 = v4
    i8 = v8

    ia1 = iatmp1
    ia2 = iatmp2
    ia4 = iatmp4
    ia8 = iatmp8

    if (i1/=min1 .or. i2/=min2 .or. i4/=min4 .or. i8/=min8) error stop 2
    if (any(ia1 /= [integer(1):: 1,1,2,3,5,8,13,21,34,v1])) error stop 3
    if (any(ia2 /= [integer(2):: v2,0,v2,0,v2,0,v2,0,v2,0])) error stop 4
    if (any(ia4 /= [integer(4):: 0,v4,0,v4,0,v4,0,v4,0,v4])) error stop 5
    if (any(ia8 /= [integer(8):: v8,0,v8,0,v8,0,v8,0,v8,0])) error stop 6

    ! now test assignment *from* coarrays (first, reset targets)
    v1 = 0
    v2 = 0
    v4 = 0
    v8 = 0
    iatmp1 = 0
    iatmp2 = 0
    iatmp4 = 0
    iatmp8 = 0

    v1 = i1
    v2 = i2
    v4 = i4
    v8 = i8
    iatmp1 = ia1
    iatmp2 = ia2
    iatmp4 = ia4
    iatmp8 = ia8

    if (v1/=min1 .or. v2/=min2 .or. v4/=min4 .or. v8/=min8) error stop 12
    if (any(iatmp1 /= [integer(1):: 1,1,2,3,5,8,13,21,34,v1])) error stop 13
    if (any(iatmp2 /= [integer(2):: v2,0,v2,0,v2,0,v2,0,v2,0])) error stop 14
    if (any(iatmp4 /= [integer(4):: 0,v4,0,v4,0,v4,0,v4,0,v4])) error stop 15
    if (any(iatmp8 /= [integer(8):: v8,0,v8,0,v8,0,v8,0,v8,0])) error stop 16

    ! now try targets - first release already allocated space, then set up pointers and set to max value
    deallocate(v1, iatmp1, v2, iatmp2, v4, iatmp4, v8, iatmp8)

    v1 => itmp1
    v2 => itmp2
    v4 => itmp4
    v8 => itmp8
    iatmp1 => iatgt1
    iatmp2 => iatgt2
    iatmp4 => iatgt4
    iatmp8 => iatgt8

    v1 = max1
    v2 = max2
    v4 = max4
    v8 = max8

    i1 = v1
    i2 = v2
    i4 = v4
    i8 = v8

    iatmp1               = [integer(1):: 1,1,2,3,5,8,13,21,34,v1]
    iatmp2               = mid2
    iatmp2(1:9:2)        = v2 ! odd elements
    iatmp4               = mid4
    iatmp4([2,4,6,8,10]) = v4 ! even elements
    iatmp8               = mid8
    iatmp8(9:1:-2)       = v8 ! odd elements again

    ia1 = iatmp1
    ia2 = iatmp2
    ia4 = iatmp4
    ia8 = iatmp8

    if (i1/=v1 .or. i2/=v2 .or. i4/=v4 .or. i8/=v8) error stop 22
    if (any(ia1 /= [integer(1):: 1,1,2,3,5,8,13,21,34,v1])) error stop 23
    if (any(ia2 /= [v2,mid2,v2,mid2,v2,mid2,v2,mid2,v2,mid2])) error stop 24
    if (any(ia4 /= [mid4,v4,mid4,v4,mid4,v4,mid4,v4,mid4,v4])) error stop 25
    if (any(ia8 /= [v8,mid8,v8,mid8,v8,mid8,v8,mid8,v8,mid8])) error stop 26

end program csPointerIntrinsic
