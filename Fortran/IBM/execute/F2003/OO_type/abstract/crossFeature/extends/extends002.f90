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
! %GROUP: extends002.f
! %VERIFY: extends002.out:extends002.vf
! %STDIN:
! %STDOUT: extends002.out
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
!*  DESCRIPTION                : Testing: Extends keyword, ensure structure components and bindings are inherited for
!*                                        abstract types, with array components
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
   
   type, abstract :: base
      integer, dimension(5) :: i = (/1,2,3,4,5/)
   contains
      procedure, pass :: printarray
      procedure(interf), pass, deferred :: printchildarray
   end type
   
   interface
      subroutine interf(a)
         import base
         class(base), intent(in) :: a
      end subroutine
   end interface
   
contains

   subroutine printarray(a)
      class(base), intent(in) :: a
      print *, a%i
   end subroutine

end module

module m2
   use m1
   
   type, extends(base) :: child
      integer, dimension(2) :: r = (/4, 6/)
   contains
      procedure, pass :: printchildarray
   end type  
   
   type, extends(child) :: gen3
   end type  
   
   class(base) , allocatable :: b1
   class(child), allocatable :: c1
   class(gen3) , allocatable :: g1
      
contains 

   subroutine printchildarray(a)
      class(child), intent(in) :: a
      print *, a%r
   end subroutine
   
end module


program extends002
   use m2
   
   allocate (b1, source = child())
   allocate (c1, source = gen3())
   allocate (g1, source = gen3())

   call b1%printarray()
   call c1%printarray()
   call g1%printarray()

   call b1%printchildarray()
   call c1%printchildarray()
   call g1%printchildarray()
   
end program
