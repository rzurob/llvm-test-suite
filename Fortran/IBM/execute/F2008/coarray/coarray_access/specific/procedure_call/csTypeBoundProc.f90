!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : csTypeBoundProc
!*
!*  DATE                       : 2010-10-04
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray access (specific) - procedure call
!*  SECONDARY FUNCTIONS TESTED : pass coarray actual arguments to type-bound procedure (*not* as the passed-object dummy argument, obviously), which passes it on and modifies or processes it in an internal procedure
!*  ADAPTED FROM               : csModuleInternalProc (<-csIntentInOutArg<-csCharacterArg<-csSimpleCharacter<-csSimpleInteger<-csSimpleLogical)
!*
!*  DESCRIPTION
!*
!*  In the main program, invoke typebound procedures to assign values to coarray
!*  scalar dummy arguments from the object on which the procedure is invoked.
!*  Note: Only intrinsic coarrays are involved.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module mod

  implicit none

  character(1), parameter :: a1 = ' ',    b1 = '~'
  character(3), parameter :: a3 = 'A9Z',  b3 = '!z~'

  type ParmSource (k,l)
     integer, kind :: k
     integer, len  :: l
     character(l)  :: cval = ''
     integer(k)    :: ival = 0
   contains
     procedure, pass :: tb41
     procedure, pass :: tb23
  end type ParmSource

contains

  subroutine tb41(arg, c1, i4)
    class(ParmSource(4,*)) :: arg
    character(1) :: c1[*]
    integer(4) :: i4[*]
    c1 = arg%cval
    i4 = arg%ival
  end subroutine tb41

  subroutine tb23(arg, c3, i2)
    class(ParmSource(2,*)) :: arg
    character(3) :: c3[*]
    integer(2) :: i2[*]
    c3 = arg%cval
    i2 = arg%ival
  end subroutine tb23

end module mod


program csTypeBoundProc

  use :: mod
  implicit none

  character(1), save :: x1[*] = ''
  character(3), save :: x3[*] = ''
  integer(4), save :: y4[*] = 0
  integer(2), save :: y2[*] = 0

  type(ParmSource(2,3)) :: p1
  type(ParmSource(4,1)) :: p2

  p1 = ParmSource(2,3)('xyz', 100+this_image())
  p2 = ParmSource(4,1)('A', 1000000+this_image())

  call p1%tb23(x3,y2)
  call p2%tb41(x1,y4)
  if (x1 /= 'A' .or. x3 /= 'xyz') error stop 2
  if (y2 /= (100+this_image()) .or. y4 /= (1000000+this_image())) error stop 3

end program csTypeBoundProc
