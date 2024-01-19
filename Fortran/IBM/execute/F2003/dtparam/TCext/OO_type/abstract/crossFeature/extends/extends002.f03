! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/extends/extends002.f
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

   type, abstract :: base(k1)    ! (4)
      integer, kind             :: k1
      integer(k1), dimension(5) :: i = (/1,2,3,4,5/)
   contains
      procedure, pass :: printarray
      procedure(interf), pass, deferred :: printchildarray
   end type

   interface
      subroutine interf(a)
         import base
         class(base(4)), intent(in) :: a
      end subroutine
   end interface

contains

   subroutine printarray(a)
      class(base(4)), intent(in) :: a
      print *, a%i
   end subroutine

end module

module m2
   use m1

   type, extends(base) :: child(k2)    ! (4,4)
      integer, kind             :: k2
      integer(k2), dimension(2) :: r = (/4, 6/)
   contains
      procedure, pass :: printchildarray
   end type

   type, extends(child) :: gen3(k3,n1)    ! (4,4,4,20)
       integer, kind :: k3
       integer, len  :: n1
   end type

   class(base(4)) , allocatable :: b1
   class(child(4,4)), allocatable :: c1
   class(gen3(4,4,4,20)) , allocatable :: g1

contains

   subroutine printchildarray(a)
      class(child(4,4)), intent(in) :: a
      print *, a%r
   end subroutine

end module


program extends002
   use m2

   allocate (b1, source = child(4,4)())
   allocate (c1, source = gen3(4,4,4,20)())
   allocate (g1, source = gen3(4,4,4,20)())

   call b1%printarray()
   call c1%printarray()
   call g1%printarray()

   call b1%printchildarray()
   call c1%printchildarray()
   call g1%printchildarray()

end program
