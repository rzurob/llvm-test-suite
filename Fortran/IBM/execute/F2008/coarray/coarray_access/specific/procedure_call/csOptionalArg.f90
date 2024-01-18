!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-10-04
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray access (specific) - procedure call
!*  SECONDARY FUNCTIONS TESTED : pass coarray actual arguments to procedure, into dummy argument with optional attribute
!*  ADAPTED FROM               : csIntentInOutArg (<-csCharacterArg<-csSimpleCharacter<-csSimpleInteger<-csSimpleLogical)
!*
!*  DESCRIPTION
!*
!*  In the main program, invoke internal procedures to assign simple values to
!*  character coarray scalars of different lengths, both as coarray dummy arguments
!*  and non-coarray.  The dummy args are OPTIONAL; we test the case where they are
!*  provided as actual arguments (and update them), and the case where the argument
!*  is omitted (but then do not update them).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program csOptionalArg

    implicit none

    character(1), parameter :: a1 = ' ',    b1 = '~'
    character(3), parameter :: a3 = 'A9Z',  b3 = '!z~'

    character(1), save :: c1[*] = ''
    character(3), save :: c3[*] = ''

    call twiddle1(.true., c1,  ' ', a1, 1)
    call twiddle1(.true., c1, a1, b1, 2)
    call twaddle1(.true., c1, b1, a1, 3)
    call twaddle1(.true., c1, a1, b1, 4)
    if (b1 /= fiddle1(.true., c1,a1)) call fail(5)
    if (a1 /= faddle1(.true., c1,b1)) call fail(6)

    call twiddle3(.true., c3, '   ', a3, 11)
    call twiddle3(.true., c3, a3, b3, 12)
    call twaddle3(.true., c3, b3, a3, 13)
    call twaddle3(.true., c3, a3, b3, 14)
    if (b3 /= fiddle3(.true., c3,a3)) call fail(15)
    if (a3 /= faddle3(.true., c3,b3)) call fail(16)


    call twiddle1(.false., exp1=' ', new1=a1, nr=17)
    call twiddle1(.false., exp1=a1, new1=b1, nr=18)
    call twaddle1(.false., exp1=b1, new1=a1, nr=19)
    call twaddle1(.false., exp1=a1, new1=b1, nr=20)
    if (a1 /= fiddle1(.false.,new1=a1)) call fail(21)
    if (b1 /= faddle1(.false.,new1=b1)) call fail(22)

    call twiddle3(.false., exp3='   ', new3=a3, nr=23)
    call twiddle3(.false., exp3=a3, new3=b3, nr=24)
    call twaddle3(.false., exp3=b3, new3=a3, nr=25)
    call twaddle3(.false., exp3=a3, new3=b3, nr=26)
    if (a3 /= fiddle3(.false., new3=a3)) call fail(27)
    if (b3 /= faddle3(.false., new3=b3)) call fail(28)


  contains

    subroutine twiddle1(pres, a1, exp1, new1, nr)
      character(1), optional :: a1
      character(1) :: exp1, new1
      integer :: nr
      logical :: pres
      if (pres .neqv. present(a1)) call fail(100+nr)
      if (.not.present(a1)) return
      if (a1 /= exp1) call fail(nr)
      a1 = new1
    end subroutine twiddle1

    subroutine twaddle1(pres, a1, exp1, new1, nr)
      character(1), optional :: a1[*]
      character(1) :: exp1, new1
      integer :: nr
      logical :: pres
      if (pres .neqv. present(a1)) call fail(100+nr)
      if (.not.present(a1)) return
      if (a1 /= exp1) call fail(nr)
      a1 = new1
    end subroutine twaddle1

    character function fiddle1(pres, a1, new1)
      character(1), optional :: a1
      character(1) :: new1
      logical :: pres
      if (pres .neqv. present(a1)) call fail(120)
      if (.not.present(a1)) then
        fiddle1 = new1
        return
      end if
      fiddle1 = a1
      a1 = new1
    end function fiddle1

    character function faddle1(pres, a1, new1)
      character(1), optional :: a1[*]
      character(1) :: new1
      logical :: pres
      if (pres .neqv. present(a1)) call fail(121)
      if (.not.present(a1)) then
        faddle1 = new1
        return
      end if
      faddle1 = a1
      a1 = new1
    end function faddle1


    subroutine twiddle3(pres, a3, exp3, new3, nr)
      character(3), optional :: a3
      character(3) :: exp3, new3
      integer :: nr
      logical :: pres
      if (pres .neqv. present(a3)) call fail(100+nr)
      if (.not.present(a3)) return
      if (a3 /= exp3) call fail(nr)
      a3 = new3
    end subroutine twiddle3

    subroutine twaddle3(pres, a3, exp3, new3, nr)
      character(3), optional :: a3[*]
      character(3) :: exp3, new3
      integer :: nr
      logical :: pres
      if (pres .neqv. present(a3)) call fail(100+nr)
      if (.not.present(a3)) return
      if (a3 /= exp3) call fail(nr)
      a3 = new3
    end subroutine twaddle3

    character(3) function fiddle3(pres, a3, new3)
      character(3), optional :: a3
      character(3) :: new3
      logical :: pres
      if (pres .neqv. present(a3)) call fail(122)
      if (.not.present(a3)) then
        fiddle3 = new3
        return
      end if
      fiddle3 = a3
      a3 = new3
    end function fiddle3

    character(3) function faddle3(pres, a3, new3)
      character(3), optional :: a3[*]
      character(3) :: new3
      logical :: pres
      if (pres .neqv. present(a3)) call fail(123)
      if (.not.present(a3)) then
        faddle3 = new3
        return
      end if
      faddle3 = a3
      a3 = new3
    end function faddle3


    subroutine fail(nr)
      integer :: nr
      print *, "Failed in test", nr
      error stop 12
    end subroutine fail

end program csOptionalArg
