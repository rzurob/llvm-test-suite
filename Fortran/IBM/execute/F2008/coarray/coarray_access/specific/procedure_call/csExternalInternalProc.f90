!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : csExternalInternalProc
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-10-04
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray access (specific) - procedure call
!*  SECONDARY FUNCTIONS TESTED : pass coarray actual arguments to external procedure, which passes it on and modifies or processes it in an internal procedure
!*  ADAPTED FROM               : csModuleInternalProc (<-csIntentInOutArg<-csCharacterArg<-csSimpleCharacter<-csSimpleInteger<-csSimpleLogical)
!*
!*  DESCRIPTIONn
!*
!*  In the main program, invoke external procedures and procedures internal to them
!*  to assign simple values to character coarray scalars of different lengths,
!*  both as coarray dummy arguments and non-coarray.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program csExternalInternalProc

  implicit none
  interface
     subroutine mod1(c1)
       character(1) :: c1[*]
     end subroutine mod1
     subroutine mod3(c3)
       character(3) :: c3[*]
     end subroutine mod3
  end interface

  character(1), save :: x1[*] = ''
  character(3), save :: x3[*] = ''

  call mod1(x1)
  call mod3(x3)

end program csExternalInternalProc



subroutine mod1(c1)
  character(1), parameter :: a1 = ' ',    b1 = '~'
  character(1) :: c1[*]

  call twiddle1(c1,  ' ', a1, 1)
  call twiddle1(c1, a1, b1, 2)
  call twaddle1(c1, b1, a1, 3)
  call twaddle1(c1, a1, b1, 4)
  if (b1 /= fiddle1(c1,a1)) call fail(5)
  if (a1 /= faddle1(c1,b1)) call fail(6)

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

end subroutine mod1


subroutine mod3(c3)
  character(3), parameter :: a3 = 'A9Z',  b3 = '!z~'
  character(3) :: c3[*]

  call twiddle3(c3, '   ', a3, 11)
  call twiddle3(c3, a3, b3, 12)
  call twaddle3(c3, b3, a3, 13)
  call twaddle3(c3, a3, b3, 14)
  if (b3 /= fiddle3(c3,a3)) call fail(15)
  if (a3 /= faddle3(c3,b3)) call fail(16)

contains

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

end subroutine mod3

subroutine fail(nr)
  integer :: nr
  print *, "Failed in test", nr
  error stop 12
end subroutine fail
