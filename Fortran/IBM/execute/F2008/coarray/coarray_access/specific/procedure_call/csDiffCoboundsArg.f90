!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-10-04
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray access (specific) - procedure call
!*  SECONDARY FUNCTIONS TESTED : scalar coarray dummy argument with same corank, but different cobounds as actual argument
!*  ADAPTED FROM               : csCharacterArg (<-csSimpleCharacter<-csSimpleInteger<-csSimpleLogical)
!*
!*  DESCRIPTION
!*
!*  In the main program, invoke internal procedures to assign simple values to
!*  character coarray scalars of different lengths as coarray dummy arguments.
!*  The codimensions of the dummy arguments are different than those of the
!*  actual arguments.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program csDiffCoboundsArg

    implicit none

    character(1), parameter :: a1 = ' ',    b1 = '~'
    character(3), parameter :: a3 = 'A9Z',  b3 = '!z~'

    character(1), save :: c1[1:2,2:3,3:4,*] = ''
    character(3), save :: c3[-1:0,1:2,0:1,0:*] = ''

    call twaddle1(c1, ' ', a1, 1)
    call twaddle1(c1, a1, b1, 2)
    if (b1 /= faddle1(c1,a1)) call fail(3)

    call twaddle3(c3, '   ', a3, 4)
    call twaddle3(c3, a3, b3, 5)
    if (b3 /= faddle3(c3,a3)) call fail(6)


  contains

    subroutine twaddle1(a1, exp1, new1, nr)
      character(1) :: a1[2,1,1,5:*], exp1, new1
      integer :: nr
      if (a1 /= exp1) call fail(nr)
      a1 = new1
    end subroutine twaddle1

    character function faddle1(a1, new1)
      character(1) :: a1[1,2,9:9,3:*], new1
      faddle1 = a1
      a1 = new1
    end function faddle1


    subroutine twaddle3(a3, exp3, new3, nr)
      character(3) :: a3[1,2,1,5:*], exp3, new3
      integer :: nr
      if (a3 /= exp3) call fail(nr)
      a3 = new3
    end subroutine twaddle3

    character(3) function faddle3(a3, new3)
      character(3) :: a3[1,1,2:3,*], new3
      faddle3 = a3
      a3 = new3
    end function faddle3


    subroutine fail(nr)
      integer :: nr
      print *, "Failed in test", nr
      error stop 12
    end subroutine fail

end program csDiffCoboundsArg
