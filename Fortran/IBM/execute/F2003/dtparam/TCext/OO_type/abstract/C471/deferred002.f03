! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/27/2004
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

   type, abstract :: base(k1)
      integer, kind :: k1
      integer(k1) :: id
   contains
      procedure(print1), deferred, pass :: print
   end type

   type, extends(base), abstract :: child(k2)
      integer, kind :: k2
   contains
      procedure(print2), deferred, pass :: print
   end type

   type, extends(child) :: gen3(k3)
      integer, kind :: k3
   contains
      procedure, pass :: print => printgen3
   end type

   interface
      subroutine print1(a)
         import base
         class(base(4)), intent(in) :: a
      end subroutine
   end interface

   interface
      subroutine print2(a)
         import child
         class(child(4,4)), intent(in) :: a
      end subroutine
   end interface

contains

   subroutine printgen3(a)
      class(gen3(4,4,4)), intent(in) :: a
      print *,a%id
   end subroutine

end module

program deferred002
   use m
   class(base(4)), pointer :: b1
   class(child(4,4)), allocatable :: c1

   allocate (b1, source = gen3(4,4,4)(3))
   allocate (c1, source = gen3(4,4,4)(4))

   call b1%print()
   call c1%print()

end program