! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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