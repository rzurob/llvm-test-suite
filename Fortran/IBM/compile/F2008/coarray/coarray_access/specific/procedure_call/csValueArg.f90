!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : csValueArg
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-10-04
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray access (specific) - procedure call
!*  SECONDARY FUNCTIONS TESTED : pass coarray actual arguments to procedure, into dummy argument with value attribute
!*  ADAPTED FROM               : csIntentInArg (<-csIntentInOutArg<-csCharacterArg<-csSimpleCharacter<-csSimpleInteger<-csSimpleLogical)
!*
!*  DESCRIPTION
!*
!*  In the main program, invoke internal procedures to examine character coarray scalars
!*  of different lengths, both as coarray dummy arguments and non-coarray.
!*  Mark the dummy args as VALUE.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program csValueArg

    implicit none

    character(1), parameter :: a1 = ' ',    b1 = '~'
    character(3), parameter :: a3 = 'A9Z',  b3 = '!z~'

    character(1), save :: c1[*] = ''
    character(3), save :: c3[*] = ''

    call twiddle1(c1,  ' ', 1)
    c1 = a1
    call twiddle1(c1, a1, 2)
    c1 = b1
    call twaddle1(c1, b1, 3)
    c1 = a1
    call twaddle1(c1, a1, 4)
    c1 = b1
    if (b1 /= fiddle1(c1)) call fail(5)
    c1 = a1
    if (a1 /= faddle1(c1)) call fail(6)

    call twiddle3(c3, '   ', 11)
    c3 = a3
    call twiddle3(c3, a3, 12)
    c3 = b3
    call twaddle3(c3, b3, 13)
    c3 = a3
    call twaddle3(c3, a3, 14)
    c3 = b3
    if (b3 /= fiddle3(c3)) call fail(15)
    c3 = a3
    if (a3 /= faddle3(c3)) call fail(16)


  contains

    subroutine twiddle1(a1, exp1, nr)
      character(1), value :: a1
      character(1) :: exp1
      integer :: nr
      if (a1 /= exp1) call fail(nr)
    end subroutine twiddle1

    subroutine twaddle1(a1, exp1, nr)
      character(1), value :: a1[*]
      character(1) :: exp1
      integer :: nr
      if (a1 /= exp1) call fail(nr)
    end subroutine twaddle1

    character function fiddle1(a1)
      character(1), value :: a1
      fiddle1 = a1
    end function fiddle1

    character function faddle1(a1)
      character(1), value :: a1[*]
      faddle1 = a1
    end function faddle1


    subroutine twiddle3(a3, exp3, nr)
      character(3), value :: a3
      character(3) :: exp3
      integer :: nr
      if (a3 /= exp3) call fail(nr)
    end subroutine twiddle3

    subroutine twaddle3(a3, exp3, nr)
      character(3), value :: a3[*]
      character(3) :: exp3
      integer :: nr
      if (a3 /= exp3) call fail(nr)
    end subroutine twaddle3

    character(3) function fiddle3(a3)
      character(3), value :: a3
      fiddle3 = a3
    end function fiddle3

    character(3) function faddle3(a3)
      character(3), value :: a3[*]
      faddle3 = a3
    end function faddle3


    subroutine fail(nr)
      integer :: nr
      print *, "Failed in test", nr
      error stop 12
    end subroutine fail

end program csValueArg
