!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : csSelectType
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-10-04
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray access (specific) - assignment
!*  SECONDARY FUNCTIONS TESTED : in SELECT TYPE construct, assign associate-name to coarray variables and vice-versa
!*  ADAPTED FROM               : csAssociate (<-csSimpleInteger<-csSimpleLogical)
!*
!*  DESCRIPTION
!*
!*  Assign simple values identified by a select type to integer coarray scalars
!*  and arrays of different kinds in the main program.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program csSelectType

    implicit none

    ! we're relying on the use of 2's complement arithmetic when we use an expression
    ! like "-huge(0_KIND)-1"
    integer(1), parameter :: min1 = -huge(0_1)-1,  max1 = huge(0_1), mid1 = 13_1
    integer(2), parameter :: min2 = -huge(0_2)-1,  max2 = huge(0_2), mid2 = 12345_2
    integer(4), parameter :: min4 = -huge(0_4)-1,  max4 = huge(0_4), mid4 = 898973451_4
    integer(8), parameter :: min8 = -huge(0_8)-1,  max8 = huge(0_8), mid8 = -12345678901234_8

    ! create interesting types for the association:
    type dt(k)
      integer, kind :: k
      integer(k) :: ik, iveck(10)
      integer(2*k) :: ik2, iveck2(10)
    end type dt
    type container
      type(dt(1)) :: c1
      type(dt(2)) :: c2
    end type container

    type(container), target :: x, xtmp

    integer(1), save :: i1[*] = 0, ia1(10)[*] = 0
    integer(2), save :: i2[*] = 0, ia2(10)[*] = 0
    integer(4), save :: i4[*] = 0, ia4(10)[*] = 0
    integer(8), save :: i8[*] = 0, ia8(10)[*] = 0

    call handle(x%c1%ik)
    call handle(x%c1%ik2)
    call handle(x%c2%ik)
    call handle(x%c2%ik2)
    call vhandle(x%c1%iveck)
    call vhandle(x%c1%iveck2)
    call vhandle(x%c2%iveck)
    call vhandle(x%c2%iveck2)

contains

  subroutine handle(p)
    class(*) :: p

    select type (v => p)

       type is (integer(1))
          v = min1  ! access value to assign
          i1 = v    ! assign value (must come from associate-name "variable")
          v = 0     ! erase value for later
          if (i1/=min1) error stop 12
          v = i1    ! receive value
          if (v/=min1) error stop 13 ! compare
          ! repeat for max:
          v = max1
          i1 = v
          v = 0
          if (i1/=max1) error stop 14
          v = i1
          if (v/=max1) error stop 15

       type is (integer(2))
          v = min2
          i2 = v
          v = 0
          if (i2/=min2) error stop 22
          v = i2
          if (v/=min2) error stop 23
          v = max2
          i2 = v
          v = 0
          if (i2/=max2) error stop 24
          v = i2
          if (v/=max2) error stop 25

       type is (integer(4))
          v = min4
          i4 = v
          v = 0
          if (i4/=min4) error stop 42
          v = i4
          if (v/=min4) error stop 43
          v = max4
          i4 = v
          v = 0
          if (i4/=max4) error stop 44
          v = i4
          if (v/=max4) error stop 45

       type is (integer(8))
          v = min8
          i8 = v
          v = 0
          if (i8/=min8) error stop 82
          v = i8
          if (v/=min8) error stop 83
          v = max8
          i8 = v
          v = 0
          if (i8/=max8) error stop 84
          v = i8
          if (v/=max8) error stop 85

    end select

  end subroutine handle


  subroutine vhandle(p)
    class(*) :: p(:)

    select type (v => p)

       type is (integer(1))
          v = [integer(1):: 1,1,2,3,5,8,13,21,34,min1]
          ia1 = v
          v = 0
          if (any(ia1/=[integer(1):: 1,1,2,3,5,8,13,21,34,min1])) error stop 16
          v = ia1
          if (any(v/=[integer(1):: 1,1,2,3,5,8,13,21,34,min1])) error stop 17

       type is (integer(2))
          ia2 = 0
          v = min2
          ia2(1:9:2) = v(1:9:2)
          v = -1
          if (any(ia2/=[integer(2):: min2,0,min2,0,min2,0,min2,0,min2,0])) error stop 26
          v = ia2
          if (any(v/=[integer(2):: min2,0,min2,0,min2,0,min2,0,min2,0])) error stop 27

       type is (integer(4))
          v = 0
          ia4 = 0
          v(10:2:-2) = min4
          ia4([2,4,6,8,10]) = v([2,4,6,8,10])
          v = 0
          if (any(ia4/=[integer(4):: 0,min4,0,min4,0,min4,0,min4,0,min4])) error stop 46
          v = ia4
          if (any(v/=[integer(4):: 0,min4,0,min4,0,min4,0,min4,0,min4])) error stop 47

       type is (integer(8))
          ia8 = 0
          v = min8
          ia8(9:1:-2) = v(1:5)
          v = 0
          if (any(ia8/=[integer(8):: min8,0,min8,0,min8,0,min8,0,min8,0])) error stop 86
          v = ia8
          if (any(v/=[integer(8):: min8,0,min8,0,min8,0,min8,0,min8,0])) error stop 87

    end select

  end subroutine vhandle

end program csSelectType
