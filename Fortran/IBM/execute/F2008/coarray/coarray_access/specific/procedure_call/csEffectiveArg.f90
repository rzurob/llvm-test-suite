!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : csEffectiveArg
!*
!*  DATE                       : 2010-10-04
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray access (specific) - procedure call
!*  SECONDARY FUNCTIONS TESTED : procedure invokes procedure with a coarray variable as the effective argument
!*  ADAPTED FROM               : csCharacterArg (<-csSimpleCharacter<-csSimpleInteger<-csSimpleLogical)
!*
!*  DESCRIPTION
!*
!*  In the main program, invoke internal procedures to invoke other procedures
!*  and they still others, ultimately assigning values to character coarray scalars
!*  of different lengths, both as coarray dummy arguments and non-coarray.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program csEffectiveArg

    implicit none

    character(1), parameter :: a1 = ' ',    b1 = '~'
    character(3), parameter :: a3 = 'A9Z',  b3 = '!z~'

    character(1), save :: c1[*] = ''
    character(3), save :: c3[*] = ''

    call twiddle1(c1,  ' ', a1, 10)
    call twiddle1(c1, a1, b1, 20)
    call twaddle1(c1, b1, a1, 30)
    call twaddle1(c1, a1, b1, 40)
    if (b1 /= fiddle1(c1,a1)) call fail(50)
    if (a1 /= faddle1(c1,b1)) call fail(60)

    call twiddle3(c3, '   ', a3, 110)
    call twiddle3(c3, a3, b3, 120)
    call twaddle3(c3, b3, a3, 130)
    call twaddle3(c3, a3, b3, 140)
    if (b3 /= fiddle3(c3,a3)) call fail(150)
    if (a3 /= faddle3(c3,b3)) call fail(160)


  contains

    subroutine twiddle1(a1, exp1, new1, nr)
      character(1) :: a1, exp1, new1
      integer :: nr
      if (a1 /= exp1) call fail(nr)
      call twiddle1a(a1, exp1, new1, nr)
    end subroutine twiddle1

    subroutine twiddle1a(a1, exp1, new1, nr)
      character(1) :: a1, exp1, new1
      integer :: nr
      if (a1 /= exp1) call fail(nr+1)
      a1 = new1
    end subroutine twiddle1a

    subroutine twaddle1(a1, exp1, new1, nr)
      character(1) :: a1[*], exp1, new1
      integer :: nr
      if (a1 /= exp1) call fail(nr)
      call twaddle1a(a1, exp1, new1, nr)
    end subroutine twaddle1

    subroutine twaddle1a(a1, exp1, new1, nr)
      character(1) :: a1[*], exp1, new1
      integer :: nr
      if (a1 /= exp1) call fail(nr+2)
      a1 = new1
    end subroutine twaddle1a

    character function fiddle1(a1, new1)
      character(1) :: a1, new1
      fiddle1 = fiddle1a(a1, new1)
    end function fiddle1

    character function fiddle1a(a1, new1)
      character(1) :: a1, new1
      fiddle1a = a1
      a1 = new1
    end function fiddle1a

    character function faddle1(a1, new1)
      character(1) :: a1[*], new1
      faddle1 = faddle1a(a1,new1)
    end function faddle1

    character function faddle1a(a1, new1)
      character(1) :: a1[*], new1
      faddle1a = a1
      a1 = new1
    end function faddle1a


    subroutine twiddle3(a3, exp3, new3, nr)
      character(3) :: a3, exp3, new3
      integer :: nr
      if (a3 /= exp3) call fail(nr)
      call twiddle3a(a3, exp3, new3, nr)
    end subroutine twiddle3

    subroutine twiddle3a(a3, exp3, new3, nr)
      character(3) :: a3, exp3, new3
      integer :: nr
      if (a3 /= exp3) call fail(nr+1)
      call twiddle3b(a3, exp3, new3, nr)
    end subroutine twiddle3a

    subroutine twiddle3b(a3, exp3, new3, nr)
      character(3) :: a3, exp3, new3
      integer :: nr
      if (a3 /= exp3) call fail(nr+2)
      a3 = new3
    end subroutine twiddle3b

    subroutine twaddle3(a3, exp3, new3, nr)
      character(3) :: a3[*], exp3, new3
      integer :: nr
      if (a3 /= exp3) call fail(nr)
      call twaddle3a(a3, exp3, new3, nr)
    end subroutine twaddle3

    subroutine twaddle3a(a3, exp3, new3, nr)
      character(3) :: a3[*], exp3, new3
      integer :: nr
      if (a3 /= exp3) call fail(nr+1)
      call twaddle3b(a3, exp3, new3, nr)
    end subroutine twaddle3a

    subroutine twaddle3b(a3, exp3, new3, nr)
      character(3) :: a3, exp3, new3
      integer :: nr
      if (a3 /= exp3) call fail(nr+2)
      a3 = new3
    end subroutine twaddle3b

    character(3) function fiddle3(a3, new3)
      character(3) :: a3, new3
      fiddle3 = fiddle3a(a3, new3)
    end function fiddle3

    character(3) function fiddle3a(a3, new3)
      character(3) :: a3, new3
      fiddle3a = fiddle3b(a3, new3)
    end function fiddle3a

    character(3) function fiddle3b(a3, new3)
      character(3) :: a3, new3
      fiddle3b = a3
      a3 = new3
    end function fiddle3b

    character(3) function faddle3(a3, new3)
      character(3) :: a3[*], new3
      faddle3 = faddle3a(a3, new3)
    end function faddle3

    character(3) function faddle3a(a3, new3)
      character(3) :: a3[*], new3
      faddle3a = faddle3b(a3, new3)
    end function faddle3a

    character(3) function faddle3b(a3, new3)
      character(3) :: a3[*], new3
      faddle3b = a3
      a3 = new3
    end function faddle3b


    subroutine fail(nr)
      integer :: nr
      print *, "Failed in test", nr
      error stop 12
    end subroutine fail

end program csEffectiveArg
