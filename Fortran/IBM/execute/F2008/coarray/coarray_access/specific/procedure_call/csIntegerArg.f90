!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : csIntegerArg
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-10-04
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray access (specific) - procedure call
!*  SECONDARY FUNCTIONS TESTED : pass INTEGER scalar coarray actual argument to procedure which examines it and modifies it
!*  ADAPTED FROM               : csLogicalArg, csSimpleInteger
!*
!*  DESCRIPTION
!*
!*  Invoke procedures with coarray actual arguments - they examine and modify the arguments.
!*  Arrays are already handled in many other features, so we skip these.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program csIntegerArg

    implicit none

    ! we're relying on the use of 2's complement arithmetic when we use an expression
    ! like "-huge(0_KIND)-1"
    integer(1), parameter :: min1 = -huge(0_1)-1,  max1 = huge(0_1), mid1 = 13_1
    integer(2), parameter :: min2 = -huge(0_2)-1,  max2 = huge(0_2), mid2 = 12345_2
    integer(4), parameter :: min4 = -huge(0_4)-1,  max4 = huge(0_4), mid4 = 898973451_4
    integer(8), parameter :: min8 = -huge(0_8)-1,  max8 = huge(0_8), mid8 = -12345678901234_8

    integer(1), save :: i1[*] = 0
    integer(2), save :: i2[*] = 0
    integer(4), save :: i4[*] = 0
    integer(8), save :: i8[*] = 0

    call twiddle1(i1,  0_1, min1, 1)
    call twiddle1(i1, min1, mid1, 2)
    call twaddle1(i1, mid1, max1, 3)
    call twaddle1(i1, max1, min1, 4)
    if (min1 /= fiddle1(i1,mid1)) call fail(5)
    if (mid1 /= faddle1(i1,max1)) call fail(6)

    call twiddle2(i2,  0_2, min2, 11)
    call twiddle2(i2, min2, mid2, 12)
    call twaddle2(i2, mid2, max2, 13)
    call twaddle2(i2, max2, min2, 14)
    if (min2 /= fiddle2(i2,mid2)) call fail(15)
    if (mid2 /= faddle2(i2,max2)) call fail(16)

    call twiddle4(i4,  0_4, min4, 21)
    call twiddle4(i4, min4, mid4, 22)
    call twaddle4(i4, mid4, max4, 23)
    call twaddle4(i4, max4, min4, 24)
    if (min4 /= fiddle4(i4,mid4)) call fail(25)
    if (mid4 /= faddle4(i4,max4)) call fail(26)

    call twiddle8(i8,  0_8, min8, 31)
    call twiddle8(i8, min8, mid8, 32)
    call twaddle8(i8, mid8, max8, 33)
    call twaddle8(i8, max8, min8, 34)
    if (min8 /= fiddle8(i8,mid8)) call fail(35)
    if (mid8 /= faddle8(i8,max8)) call fail(36)


  contains

    subroutine twiddle1(a1, exp1, new1, nr)
      integer(1) :: a1, exp1, new1
      integer :: nr
      if (a1 /= exp1) call fail(nr)
      a1 = new1
    end subroutine twiddle1

    subroutine twaddle1(a1, exp1, new1, nr)
      integer(1) :: a1[*], exp1, new1
      integer :: nr
      if (a1 /= exp1) call fail(nr)
      a1 = new1
    end subroutine twaddle1

    integer function fiddle1(a1, new1)
      integer(1) :: a1, new1
      fiddle1 = a1
      a1 = new1
    end function fiddle1

    integer function faddle1(a1, new1)
      integer(1) :: a1[*], new1
      faddle1 = a1
      a1 = new1
    end function faddle1


    subroutine twiddle2(a2, exp2, new2, nr)
      integer(2) :: a2, exp2, new2
      integer :: nr
      if (a2 /= exp2) call fail(nr)
      a2 = new2
    end subroutine twiddle2

    subroutine twaddle2(a2, exp2, new2, nr)
      integer(2) :: a2[*], exp2, new2
      integer :: nr
      if (a2 /= exp2) call fail(nr)
      a2 = new2
    end subroutine twaddle2

    integer function fiddle2(a2, new2)
      integer(2) :: a2, new2
      fiddle2 = a2
      a2 = new2
    end function fiddle2

    integer function faddle2(a2, new2)
      integer(2) :: a2[*], new2
      faddle2 = a2
      a2 = new2
    end function faddle2


    subroutine twiddle4(a4, exp4, new4, nr)
      integer(4) :: a4, exp4, new4
      integer :: nr
      if (a4 /= exp4) call fail(nr)
      a4 = new4
    end subroutine twiddle4

    subroutine twaddle4(a4, exp4, new4, nr)
      integer(4) :: a4[*], exp4, new4
      integer :: nr
      if (a4 /= exp4) call fail(nr)
      a4 = new4
    end subroutine twaddle4

    integer function fiddle4(a4, new4)
      integer(4) :: a4, new4
      fiddle4 = a4
      a4 = new4
    end function fiddle4

    integer function faddle4(a4, new4)
      integer(4) :: a4[*], new4
      faddle4 = a4
      a4 = new4
    end function faddle4


    subroutine twiddle8(a8, exp8, new8, nr)
      integer(8) :: a8, exp8, new8
      integer :: nr
      if (a8 /= exp8) call fail(nr)
      a8 = new8
    end subroutine twiddle8

    subroutine twaddle8(a8, exp8, new8, nr)
      integer(8) :: a8[*], exp8, new8
      integer :: nr
      if (a8 /= exp8) call fail(nr)
      a8 = new8
    end subroutine twaddle8

    integer function fiddle8(a8, new8)
      integer(8) :: a8, new8
      fiddle8 = a8
      a8 = new8
    end function fiddle8

    integer function faddle8(a8, new8)
      integer(8) :: a8[*], new8
      faddle8 = a8
      a8 = new8
    end function faddle8

    subroutine fail(nr)
      integer :: nr
      print *, "Failed in test", nr
      error stop 12
    end subroutine fail

end program csIntegerArg
