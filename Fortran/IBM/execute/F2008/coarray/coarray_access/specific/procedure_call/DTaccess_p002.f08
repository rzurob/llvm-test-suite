!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : May 2011
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray access (specific) - procedure call
!*
!*  DESCRIPTION
!*
!*  Invoke procedures with Derived Type coarray integer component arguments (examine and modify them)
!*  Arrays are already handled in many other features, so we skip these.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

	implicit none

	integer(1), parameter :: min1 = -huge(0_1)-1,  max1 = huge(0_1), mid1 = 13_1
	integer(2), parameter :: min2 = -huge(0_2)-1,  max2 = huge(0_2), mid2 = 12345_2
	integer(4), parameter :: min4 = -huge(0_4)-1,  max4 = huge(0_4), mid4 = 898973451_4
	integer(8), parameter :: min8 = -huge(0_8)-1,  max8 = huge(0_8), mid8 = -12345678901234_8

	type obj
		integer(1) :: i1
		integer(2) :: i2
		integer(4) :: i4
		integer(8) :: i8
	end type
	type (obj), save :: caf[*]

	caf = obj(0, 0, 0, 0)

	call twiddle1(caf%i1, 0_1, min1, 1)
	call twiddle1(caf%i1, min1, mid1, 2)
	call twaddle1(caf, mid1, max1, 3)
	call twaddle1(caf, max1, min1, 4)
	if (min1 /= fiddle1(caf%i1,mid1)) then
		print *, "expected", min1
		error stop 28
	end if
	if (mid1 /= faddle1(caf,max1)) then
		print *, "expected", mid1
		error stop 29
	end if

	call twiddle2(caf%i2, 0_2, min2, 11)
	call twiddle2(caf%i2, min2, mid2, 12)
	call twaddle2(caf, mid2, max2, 13)
	call twaddle2(caf, max2, min2, 14)
	if (min2 /= fiddle2(caf%i2,mid2)) then
		print *, "expected", min2
		error stop 38
	end if
	if (mid2 /= faddle2(caf,max2)) then
		print *, "expected", mid2
		error stop 39
	end if

	call twiddle4(caf%i4, 0_4, min4, 21)
	call twiddle4(caf%i4, min4, mid4, 22)
	call twaddle4(caf, mid4, max4, 23)
	call twaddle4(caf, max4, min4, 24)
	if (min4 /= fiddle4(caf%i4,mid4)) then
		print *, "expected", min4
		error stop 48
	end if
	if (mid4 /= faddle4(caf,max4)) then
		print *, "expected", mid4
		error stop 49
	end if

	call twiddle8(caf%i8, 0_8, min8, 31)
	call twiddle8(caf%i8, min8, mid8, 32)
	call twaddle8(caf, mid8, max8, 33)
	call twaddle8(caf, max8, min8, 34)
	if (min8 /= fiddle8(caf%i8,mid8)) then
		print *, "expected", min8
		error stop 58
	end if
	if (mid8 /= faddle8(caf,max8)) then
		print *, "expected", mid8
		error stop 59
	end if


  contains

    subroutine twiddle1(a1, exp1, new1, nr)
      integer(1) :: a1, exp1, new1
      integer :: nr

      if (a1 /= exp1) then
      	print *, a1, exp1
      	call fail(nr)
      end if
      a1 = new1
    end subroutine twiddle1

    subroutine twaddle1(a1, exp1, new1, nr)
      type (obj) :: a1[*]
      integer(1) :: exp1, new1
      integer :: nr

      if (a1%i1 /= exp1) then
      	print *, a1%i1, exp1
      	call fail(nr)
      end if
      a1%i1 = new1
    end subroutine twaddle1

    integer(1) function fiddle1(a1, new1)
      integer(1) :: a1, new1
      fiddle1 = a1
      a1 = new1
    end function fiddle1

    integer(1) function faddle1(a1, new1)
      type (obj) :: a1[*]
      integer(1) :: new1
      faddle1 = a1%i1
      a1%i1 = new1
    end function faddle1


    subroutine twiddle2(a2, exp2, new2, nr)
      integer(2) :: a2, exp2, new2
      integer :: nr

      if (a2 /= exp2) then
      	print *, a2, exp2
      	call fail(nr)
      end if
      a2 = new2
    end subroutine twiddle2

    subroutine twaddle2(a2, exp2, new2, nr)
	 type (obj) :: a2[*]
      integer(2) :: exp2, new2
      integer :: nr

      if (a2%i2 /= exp2) then
      	print *, a2%i2, exp2
      	call fail(nr)
      end if
      a2%i2 = new2
    end subroutine twaddle2

    integer(2) function fiddle2(a2, new2)
      integer(2) :: a2, new2
      fiddle2 = a2
      a2 = new2
    end function fiddle2

    integer(2) function faddle2(a2, new2)
      type (obj) :: a2[*]
      integer(2) :: new2
      faddle2 = a2%i2
      a2%i2 = new2
    end function faddle2


    subroutine twiddle4(a4, exp4, new4, nr)
      integer(4) :: a4, exp4, new4
      integer :: nr

      if (a4 /= exp4) then
      	print *, a4, exp4
      	call fail(nr)
      end if
      a4 = new4
    end subroutine twiddle4

    subroutine twaddle4(a4, exp4, new4, nr)
      type (obj) :: a4[*]
      integer(4) :: exp4, new4
      integer :: nr

      if (a4%i4 /= exp4) then
      	print *, a4%i4, exp4
      	call fail(nr)
      end if
      a4%i4 = new4
    end subroutine twaddle4

    integer(4) function fiddle4(a4, new4)
      integer(4) :: a4, new4
      fiddle4 = a4
      a4 = new4
    end function fiddle4

    integer(4) function faddle4(a4, new4)
      type (obj) :: a4[*]
      integer(4) :: new4
      faddle4 = a4%i4
      a4%i4 = new4
    end function faddle4


    subroutine twiddle8(a8, exp8, new8, nr)
      integer(8) :: a8, exp8, new8
      integer :: nr

      if (a8 /= exp8) then
      	print *, a8, exp8
      	call fail(nr)
      end if
      a8 = new8
    end subroutine twiddle8

    subroutine twaddle8(a8, exp8, new8, nr)
      type (obj) :: a8[*]
      integer(8) :: exp8, new8
      integer :: nr

      if (a8%i8 /= exp8) then
      	print *, a8%i8, exp8
      	call fail(nr)
      end if
      a8%i8 = new8
    end subroutine twaddle8

    integer(8) function fiddle8(a8, new8)
      integer(8) :: a8, new8
      fiddle8 = a8
      a8 = new8
    end function fiddle8

    integer(8) function faddle8(a8, new8)
      type (obj) :: a8[*]
      integer(8) :: new8
      faddle8 = a8%i8
      a8%i8 = new8
    end function faddle8

    subroutine fail(nr)
      integer :: nr
      print *, "Failed in test", nr
      error stop 12
    end subroutine fail

end

