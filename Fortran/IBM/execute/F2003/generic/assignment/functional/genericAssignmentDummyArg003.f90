!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: polymorphic dummy arguments being the operand
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
      integer(4) :: i = -99
      contains
         procedure, pass :: bamt
         generic :: assignment(=) => bamt
   end type

   type, extends(base) :: child
      integer(4) :: j = -999
   end type

   contains

   subroutine bamt ( a, b )
      class(base), intent(out) :: a
      class(base), intent(in) :: b

      a%i = b%i + 1
      select type ( a )
         type is ( child )
            select type ( b )
               type is ( child )
                  a%j = b%j + 1
            end select
      end select

      print *, 'bamt'

   end subroutine

   subroutine assignment(a, b)
      class(base), intent(inout)  :: a
      class(base), intent(in)  :: b

      print *, 'assignment'
      a = b

   end subroutine
end module

program genericAssignmentDummyArg003
   use m

   class(base), allocatable :: b1
   type(base) :: b2
   class(child), pointer :: c1
   type(child) :: c2

   allocate ( b1, c1 )

   call assignment( b1 , base(100) )
   call assignment( b2 , base(200) )
   call assignment( c1, child(1000, 2000) )
   call assignment( c2, child(3000, 4000) )

   print *, b1%i
   print *, b2%i
   print *, c1%i, c1%j
   print *, c2%i, c2%j

   call assignment ( b1, c2 )
   call assignment ( c2, b2 )

   print *, b1%i
   print *, c2%i, c2%j

   deallocate ( b1 )

   allocate ( child :: b1 )
   call assignment ( b1, c1 )

   select type ( b1 )
      type is ( child )
         print *, b1
   end select

   call assignment ( c2, b1 )
   print *, c2%i, c2%j


end program


