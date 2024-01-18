!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : csIntentOutArg
!*
!*  DATE                       : 2010-10-04
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray access (specific) - procedure call
!*  SECONDARY FUNCTIONS TESTED : pass coarray actual arguments into intent(out) dummy argument of procedure
!*  ADAPTED FROM               : csIntentInOutArg (<-csCharacterArg<-csSimpleCharacter<-csSimpleInteger<-csSimpleLogical)
!*
!*  DESCRIPTION
!*
!*  In the main program, invoke internal procedures to assign simple values to
!*  character coarray scalars of different lengths, both as coarray dummy arguments
!*  and non-coarray.  Mark the dummy args as INTENT(OUT).
!*  Note: Because the dummy args are "out", we cannot read the value passed in,
!*  So we need to verify differently.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program csIntentOutArg

    implicit none

    character(1), parameter :: a1 = ' ',    b1 = '~'
    character(3), parameter :: a3 = 'A9Z',  b3 = '!z~'

    character(1), save :: c1[*] = ''
    character(3), save :: c3[*] = ''

    call twiddle1(c1, a1)
    if (c1 /= a1) error stop 2

    call twiddle1(c1, b1)
    if (c1 /= b1) error stop 3

    call twaddle1(c1, a1)
    if (c1 /= a1) error stop 4

    call twaddle1(c1, b1)
    if (c1 /= b1) error stop 5

    if (a1 /= fiddle1(c1,a1)) error stop 6
    if (b1 /= faddle1(c1,b1)) error stop 7

    call twiddle3(c3, a3)
    if (c3 /= a3) error stop 12

    call twiddle3(c3, b3)
    if (c3 /= b3) error stop 13

    call twaddle3(c3, a3)
    if (c3 /= a3) error stop 14

    call twaddle3(c3, b3)
    if (c3 /= b3) error stop 15

    if (a3 /= fiddle3(c3,a3)) error stop 16
    if (b3 /= faddle3(c3,b3)) error stop 17


  contains

    subroutine twiddle1(a1, new1)
      character(1), intent(out) :: a1
      character(1) :: new1
      a1 = new1
    end subroutine twiddle1

    subroutine twaddle1(a1, new1)
      character(1), intent(out) :: a1[*]
      character(1) :: new1
      a1 = new1
    end subroutine twaddle1

    character function fiddle1(a1, new1)
      character(1), intent(out) :: a1
      character(1) :: new1
      a1 = new1
      fiddle1 = a1
    end function fiddle1

    character function faddle1(a1, new1)
      character(1), intent(out) :: a1[*]
      character(1) :: new1
      a1 = new1
      faddle1 = a1
    end function faddle1


    subroutine twiddle3(a3, new3)
      character(3), intent(out) :: a3
      character(3) :: new3
      a3 = new3
    end subroutine twiddle3

    subroutine twaddle3(a3, new3)
      character(3), intent(out) :: a3[*]
      character(3) :: new3
      a3 = new3
    end subroutine twaddle3

    function fiddle3(a3, new3)
      character(3), intent(out) :: a3
      character(3) :: new3, fiddle3
      a3 = new3
      fiddle3 = a3
    end function fiddle3

    function faddle3(a3, new3)
      character(3), intent(out) :: a3[*]
      character(3) :: new3, faddle3
      a3 = new3
      faddle3 = a3
    end function faddle3

end program csIntentOutArg
