 !#######################################################################
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: dummy013.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  C503 The TYPE(derived-type-spec) shall not specify an abstract type
!*                                         VALUE attribute (only want to make sure it works with abstract type, will not go into detail testing on optional attribute)
!*                                            b) type bound procedure
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

module m

   type, abstract :: base
      integer :: id
   contains
      procedure, pass :: getidplus1
   end type

   type, extends(base) :: child
   end type

contains

   integer function getidplus1(a)
      class(base) :: a
      getidplus1 = a%id + 1
   end function

end module

program dummy013
   use m

   class(base), allocatable :: b1
   class(child), allocatable :: c1

   allocate (b1, source = child(4))
   allocate (c1, source = child(3))

   if ((b1%getidplus1() .ne. 5) .or. (b1%id .ne. 4) ) error stop 1_4
   if ((c1%getidplus1() .ne. 4) .or. (c1%id .ne. 3) ) error stop 2_4

end program
