!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : assignment: operands with poly scalar
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

   type base
      integer :: i
      contains
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type


   type, extends(base) :: child
   end type

   contains

      subroutine bassgn ( a, b )
         class(base), intent(out) :: a
         class(base), intent(in) :: b

         a%i = b%i
         print *, a%i, '=', b%i

      end subroutine

end module


program genericAssignmentScalar007
   use m

   type(base) :: b1
   class(base), pointer :: b2
   class(base), allocatable :: b3
   class(child), pointer :: c1
   class(child), allocatable :: c2
   type(child) :: c3

   b1 = base(10)
   allocate ( b2, b3, c1, c2 )
   c3 = base( 60 )
   b2 = child(20)
   b3 = base(30)
   c1 = base(40)
   c2 = child(50)

   if ( ( b1%i /= 10 ) .or. &
        ( b2%i /= 20 ) .or. &
        ( b3%i /= 30 ) .or. &
        ( c1%i /= 40 ) .or. &
        ( c2%i /= 50 ) .or. &
        ( c3%i /= 60 ) ) error stop 1_4

   b1 = c1
   b2 = c2
   b3 = c3
   c1 = b3
   c2 = b1
   c3 = b2

   if ( ( b1%i /= 40 ) .or. &
        ( b2%i /= 50 ) .or. &
        ( b3%i /= 60 ) .or. &
        ( c1%i /= 60 ) .or. &
        ( c2%i /= 40 ) .or. &
        ( c3%i /= 50 ) ) error stop 2_4

   b1 = b2
   b2 = b3
   b3 = b1
   c1 = c2
   c2 = c3
   c3 = c1

   if ( ( b1%i /= 50 ) .or. &
        ( b2%i /= 60 ) .or. &
        ( b3%i /= 50 ) .or. &
        ( c1%i /= 40 ) .or. &
        ( c2%i /= 50 ) .or. &
        ( c3%i /= 40 ) ) error stop 3_4

end program
