! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  User-defined operator and assignment
!*                               User-defined assignment
!*                                  a) both left and right hand side of the assignment are poly abstract type
!*                                     Left         Right
!*                                     Abstract     Abstract
!*                                     Abstract     non-Abstract
!*                                     non-Abstract Abstract
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

program userDefOp006
   use m

   class(base), allocatable :: b1, b2, b3
   type(child), allocatable :: c1
   class(child), allocatable :: c2

   allocate( b1, source=child(1) )
   allocate( b2, source=child(2) )
   allocate( b3, source=child(3) )
   allocate( c1, source=child(4) )
   allocate( c2, source=child(5) )

   b1 = b2
   b2 = c1
   b3 = c2
   c1 = b3
   c2 = b2

   if ( b1%id .ne. 2 ) error stop 1_4
   if ( b2%id .ne. 4 ) error stop 2_4
   if ( b3%id .ne. 5 ) error stop 3_4
   if ( c1%id .ne. 5 ) error stop 4_4
   if ( c2%id .ne. 4 ) error stop 5_4

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
