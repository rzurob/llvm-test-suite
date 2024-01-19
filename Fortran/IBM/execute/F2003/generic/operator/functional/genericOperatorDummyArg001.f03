!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator
!*
!*  DESCRIPTION                : operator: non-poly dummy arguments being the operand
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
      integer(4) :: i
      contains
         procedure, pass :: badd
         generic :: operator(+) => badd
   end type

   interface
      type(base) function badd ( a, b )
         import base
         class(base), intent(in) :: a
         type(base), intent(in) :: b
      end function
   end interface

end module

program genericOperatorDummyArg001
   use m

   type(base) :: b1, b2
   type(base), pointer :: b3

   b1 = add ( base(100) , base(50) )
   print *, b1%i

   b2 = add( add ( base(200) , base(300) ) , base(400)  )
   print *, b2%i

   allocate ( b3 )

   b3 = add( b1, b2 )
   print *, b3%i

   b2 = add ( b2, base(200) )
   print *, b2%i

   contains

      type(base) function add(a, b)
         type(base), intent(in) :: a
         type(base), intent(in)  :: b

         print *, 'add'
         add = a+ b

      end function

end program

type(base) function badd ( a, b )
   use m, only: base
   class(base), intent(in) :: a
   type(base), intent(in) :: b

   badd%i = a%i + b%i

   print *, 'badd'

end function
