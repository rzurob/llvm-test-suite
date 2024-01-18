!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: funcRetrn003.f
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
!*  DESCRIPTION                : Testing: Function subprogram (Section 12.5.2.1), function return cannot be abstract type, class(abstract type)
!*                                        returns extension of abstract type
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

   type(child) function foo(a)
      pointer :: foo
      type(child), intent(in) :: a
      allocate(foo, source=a)
   end function

   function foo1(a) result (boo)
      class(child), pointer :: boo
      type(child), intent(in) :: a
      allocate(boo, source=a )
   end function

end module

program funcRetrn003
   use m

   class(base), allocatable :: c
   class(child), allocatable :: c1

   allocate (c1, source = child(4))

   allocate ( c,source=foo(c1) )
   if (c%id .ne. 4) error stop 1_4

   deallocate (c)
   allocate ( c, source=foo1(child(5)) )

   if (c%id .ne. 5) error stop 2_4

end program

