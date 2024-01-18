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
!*                                  1) scalar abstract declared types
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

program userDefOp001
   use m

   class(base), allocatable :: b1, b2, b3, b4

   allocate( b1, source=child(3) )
   allocate( b2, source=child(1) )
   allocate( b3, source=(b1+b2) )
   allocate( b4, source=(b1+b2+b3) )

   if ( b3%id .ne. 4 ) error stop 1_4
   if ( b4%id .ne. 8 ) error stop 2_4

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
