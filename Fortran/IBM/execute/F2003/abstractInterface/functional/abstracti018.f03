! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: C471 An overriding binding shall have the DEFERRED attribute only if the binding
!*                                        it overrides is deferred.
!*                                        Overridding a deferred binding with a deferred binding and non-deferred binding
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
   contains
      procedure(print1), deferred, pass :: print
   end type

   type, extends(base), abstract :: child
   contains
      procedure(print2), deferred, pass :: print
   end type

   type, extends(child) :: gen3
   contains
      procedure, pass :: print => printgen3
   end type

   abstract interface
      subroutine print1(a)
         import base
         class(base), intent(in) :: a
      end subroutine
   end interface

   abstract interface
      subroutine print2(a)
         import child
         class(child), intent(in) :: a
      end subroutine
   end interface

contains

   subroutine printgen3(a)
      class(gen3), intent(in) :: a
      print *,a%id
   end subroutine

end module

program abstracti018
   use m
   class(base), pointer :: b1
   class(child), allocatable :: c1

   allocate (b1, source = gen3(3))
   allocate (c1, source = gen3(4))

   call b1%print()
   call c1%print()

end program abstracti018