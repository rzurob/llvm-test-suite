!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : abstracti018lk
!*
!*                               by Alberto Alvarez-Mesquida)
!*  DATE                       : 2007-10-25 (original: 02/20/2006)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Abstract Interface
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DESCRIPTION                : Testing: C471 An overriding binding shall
!*  have the DEFERRED attribute only if the binding it overrides is deferred.
!*  Overridding a deferred binding with a deferred binding and non-deferred
!*  binding
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*
!* =====================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*
!* =====================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type, abstract :: base
      integer :: id
   contains
      procedure(print1), deferred, pass :: print
   end type

   type, extends(base), abstract :: child (lchild_1,kchild_1) !  lchild_1=5,kchild_1=2
      integer(4), len :: lchild_1
      integer, kind :: kchild_1
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
         class(child(*,2)), intent(in) :: a ! tcx: (*,2)
      end subroutine
   end interface

contains

   subroutine printgen3(a)
      class(gen3(*,2)), intent(in) :: a ! tcx: (*,2)
      print *,a%id
   end subroutine

end module

program abstracti018lk
   use m
   class(base), pointer :: b1
   class(child(:,2)), allocatable :: c1 ! tcx: (:,2)

   allocate (b1, source = gen3(5,2)(3)) ! tcx: (5,2)
   allocate (c1, source = gen3(5,2)(4)) ! tcx: (5,2)

   call b1%print()
   call c1%print()

end program abstracti018lk


! Extensions to introduce derived type parameters:
! type: child - added parameters (lchild_1,kchild_1) to invoke with (2,5) / declare with (:,2)/(*,2) - 5 changes
