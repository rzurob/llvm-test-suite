!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-10-04
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray access (specific) - procedure call
!*  SECONDARY FUNCTIONS TESTED : pass coarray actual arguments into intent(inout) dummy argument of procedure
!*  ADAPTED FROM               : csCharacterArg (<-csSimpleCharacter<-csSimpleInteger<-csSimpleLogical)
!*
!*  DESCRIPTION
!*
!*  In the main program, invoke internal procedures to assign simple values to
!*  character coarray scalars of different lengths, both as coarray dummy arguments
!*  and non-coarray.  Mark the dummy args as INTENT(INOUT).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program csIntentInOutArg

    implicit none

    character(1), parameter :: a1 = ' ',    b1 = '~'
    character(3), parameter :: a3 = 'A9Z',  b3 = '!z~'

    character(1), save :: c1[*] = ''
    character(3), save :: c3[*] = ''

    call twiddle1(c1,  ' ', a1, 1)
    call twiddle1(c1, a1, b1, 2)
    call twaddle1(c1, b1, a1, 3)
    call twaddle1(c1, a1, b1, 4)
    if (b1 /= fiddle1(c1,a1)) call fail(5)
    if (a1 /= faddle1(c1,b1)) call fail(6)

    call twiddle3(c3, '   ', a3, 11)
    call twiddle3(c3, a3, b3, 12)
    call twaddle3(c3, b3, a3, 13)
    call twaddle3(c3, a3, b3, 14)
    if (b3 /= fiddle3(c3,a3)) call fail(15)
    if (a3 /= faddle3(c3,b3)) call fail(16)


  contains

    subroutine twiddle1(a1, exp1, new1, nr)
      character(1), intent(inout) :: a1
      character(1) :: exp1, new1
      integer :: nr
      if (a1 /= exp1) call fail(nr)
      a1 = new1
    end subroutine twiddle1

    subroutine twaddle1(a1, exp1, new1, nr)
      character(1), intent(inout) :: a1[*]
      character(1) :: exp1, new1
      integer :: nr
      if (a1 /= exp1) call fail(nr)
      a1 = new1
    end subroutine twaddle1

    character function fiddle1(a1, new1)
      character(1), intent(inout) :: a1
      character(1) :: new1
      fiddle1 = a1
      a1 = new1
    end function fiddle1

    character function faddle1(a1, new1)
      character(1), intent(inout) :: a1[*]
      character(1) :: new1
      faddle1 = a1
      a1 = new1
    end function faddle1


    subroutine twiddle3(a3, exp3, new3, nr)
      character(3), intent(inout) :: a3
      character(3) :: exp3, new3
      integer :: nr
      if (a3 /= exp3) call fail(nr)
      a3 = new3
    end subroutine twiddle3

    subroutine twaddle3(a3, exp3, new3, nr)
      character(3), intent(inout) :: a3[*]
      character(3) :: exp3, new3
      integer :: nr
      if (a3 /= exp3) call fail(nr)
      a3 = new3
    end subroutine twaddle3

    character(3) function fiddle3(a3, new3)
      character(3), intent(inout) :: a3
      character(3) :: new3
      fiddle3 = a3
      a3 = new3
    end function fiddle3

    character(3) function faddle3(a3, new3)
      character(3), intent(inout) :: a3[*]
      character(3) :: new3
      faddle3 = a3
      a3 = new3
    end function faddle3


    subroutine fail(nr)
      integer :: nr
      print *, "Failed in test", nr
      error stop 12
    end subroutine fail

end program csIntentInOutArg
