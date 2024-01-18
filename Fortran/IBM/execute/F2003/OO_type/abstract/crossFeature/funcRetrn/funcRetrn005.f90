!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: funcRetrn005.f
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
!*  DESCRIPTION                : Testing: Function subprogram (Section 12.5.2.1), class(abstract type)
!*                                        optional polymorphic abstract type dummy argument
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

   type, abstract :: base
      integer :: id
   end type

   type, extends(base) :: child
   end type


contains

   class(base) function foo(a,b)
      class(base), intent(in) :: a
      class(base), optional, intent(in):: b
      pointer :: foo
      if (present(b)) then
         allocate (foo, source = b)
      else
         allocate (foo, source = a)
      end if
   end function

end module

program funcRetrn005
   use m

   class(base), pointer :: c1
   class(base), pointer :: c2

   allocate(c1, source=child(5) )

   c2 => foo(c1)
   if (c2%id .ne. 5) error stop 1_4

   c1 => foo(c1,child(7))
   if (c1%id .ne. 7) error stop 2_4

   allocate(c2, source = foo(child(2), child(3)) )
   if (c2%id .ne. 3) error stop 3_4

end program

