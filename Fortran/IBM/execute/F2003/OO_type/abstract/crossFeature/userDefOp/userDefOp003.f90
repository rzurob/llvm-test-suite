! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  User-defined operator and assignment
!*                               a) both operands are polymorphic abstract type for the operator in the interface and supplying
!*                                  3) scalar extension types
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

module m

   type, abstract :: base
      integer :: id
   end type

   type, extends(base) :: child
   end type

   interface operator(+)
      type(child) function myAdd1(a,b)
         import base, child
         class(base), intent(in) :: a, b
      end function
   end interface

   interface assignment(=)
      subroutine myAsgn1(a,b)
         import base, child
         class(base), intent(out) :: a
         class(base), intent(in)  :: b
      end subroutine
   end interface

end module

program userDefOp003
   use m

   class(child), allocatable :: c1, c2, c3, c4

   allocate(c1, source=child(3) )
   allocate(c2, source=child(1) )
   allocate(c3, source=(c1+c2) )
   allocate( c4, source=(c1+c2+c3) )

   if ( c3%id .ne. 4 ) error stop 1_4
   if ( c4%id .ne. 8 ) error stop 2_4

end program

type(child) function myAdd1(a,b)
   use m, only: base, child
   class(base), intent(in) :: a, b
   myAdd1%id = a%id + b%id
end function

subroutine myAsgn1(a,b)
   use m, only: base, child
   class(base), intent(out) :: a
   class(base), intent(in)  :: b
   a%id = b%id
end subroutine
