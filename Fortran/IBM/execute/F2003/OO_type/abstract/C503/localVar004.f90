!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: localVar004.f
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
!*  DESCRIPTION                : Testing: polymorphic abstract type entities in main program (scalar, array, pointer, allocatable)
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type , abstract:: base
      integer :: i = 5
   end type

   type, extends(base) :: child
   end type

end module

program localVar004
   use m

   class(base), pointer :: b3
   class(base), allocatable, dimension(:) :: b4

   allocate( child :: b4(4) )
   allocate(b3, source =child(4) )

   if (b3%i .ne. 4 ) error stop 1_4
   if (size(b4) .ne. 4 ) error stop 2_4

end program

