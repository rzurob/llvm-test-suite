 !#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: dummy004.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 09/28/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing:  C503 The TYPE(derived-type-spec) shall not specify an abstract type		    
!*					   dummy argument defined to be abstract type.
!*                                         polymorphic non-abstract type dummy argument (non-pointer and non-allocatable) with
!*                                         a)polymorphic non-abstract type actual argument
!*                                         b)polymorphic extension type (abstract) of non-abstract type actual argument
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

module m
   
   type :: base
      integer :: id
   end type
   
   type, abstract, extends(base) :: child
      integer :: rid
   end type
   
   type, extends(child) :: gen3
   end type

contains

   subroutine foo(a)
      class(base) :: a
      if (a%id .ne. 3)   error stop 1_4
   end subroutine

   integer function boo(a)
      class(base) :: a
      boo = a%id
   end function

end module

program dummy004
   use m
   
   class(base), allocatable :: b1
   class(child), allocatable, target :: c1
   class(child), pointer :: c2
   
   allocate (b1, source = base(3))
   allocate (c1, source = gen3(3,4))
   c2 => c1
      
   call foo(b1)
   call foo(c1)
   call foo(c2)
   
   if ( boo(b1) .ne. 3 ) error stop 2_4
   if ( boo(c1) .ne. 3 ) error stop 3_4
   if ( boo(c2) .ne. 3 ) error stop 4_4
   
end program