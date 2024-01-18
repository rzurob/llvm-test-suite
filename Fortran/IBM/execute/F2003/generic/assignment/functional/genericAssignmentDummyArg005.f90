!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: poly (assumed-shaped) array dummy arguments being the operand
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
      integer(4) :: i = -999
      contains
         procedure, pass :: bamt
         generic :: assignment(=) => bamt
   end type

   type, extends(base) :: child
      integer(4) :: j =-999
   end type

   interface
      subroutine bamt ( a, b )
         import base
         class(base), intent(out) :: a
         class(base), intent(in) :: b(:)
      end subroutine
   end interface

end module

program genericAssignmentDummyArg005
   use m

   class(base), allocatable :: b1, b2(:)
   type(child), allocatable :: c1, c2(:)

   allocate ( b1, b2(3), c1, c2(5) )

   b2(1)%i = 100
   b2(2)%i = 200
   b2(3)%i = 300

   c2(1)%i = 100
   c2(2)%i = 200
   c2(3)%i = 300
   c2(4)%i = 400
   c2(5)%i = 500
   c2(1)%j = 1000
   c2(2)%j = 2000
   c2(3)%j = 3000
   c2(4)%j = 4000
   c2(5)%j = 5000

   b1 = b2

   c1 = c2

   print *, b1%i
   print *, c1%i, c1%j

   deallocate ( b1 )
   allocate ( child :: b1 )

   b1 = b2
   select type ( b1 )
      type is ( child )
         print *, b1%i, b1%j
   end select

   b1 = c2
   select type ( b1 )
      type is ( child )
         print *, b1%i, b1%j
   end select

   c1 = b2
   print *, c1%i, c1%j

   contains

      subroutine assignment(a, b)
         type(base), intent(inout) :: a
         type(base), intent(in)  :: b(:)

         print *, 'assignment'
         a = b

      end subroutine

      subroutine assignment1(a, b)
         type(base), intent(inout) :: a
         type(base), intent(in)  :: b(-1:)

         print *, 'assignment1'
         a = b

      end subroutine

end program

subroutine bamt ( a, b )
   use m, only: base, child
   class(base), intent(out) :: a
   class(base), intent(in) :: b(:)

   a%i = b(1)%i
   do k = 2,size(b)
      a%i = b(k)%i + a%i
   end do

   select type ( a )
      type is ( child )
         select type ( b )
            type is ( child )
               a%j = b(1)%j
               do k = 2,size(b)
                  a%j = b(k)%j + a%j
               end do
         end select
    end select


   print *, 'bamt'

end subroutine
