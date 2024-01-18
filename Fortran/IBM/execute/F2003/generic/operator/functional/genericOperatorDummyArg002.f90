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
!*  SECONDARY FUNCTIONS TESTED : with Operator
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : operator: non-poly pointer or allocatable dummy arguments being the operand
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

program genericOperatorDummyArg002
   use m

   type(base), pointer :: b1
   type(base), allocatable, target :: b2

   allocate ( b1, source = base ( 100 ))
   b1 = add ( b1, b2 )
   print *, b1%i

   deallocate ( b2 )

   b1 = base(b1%i/110)

   b2 = add ( b1, b2 )
   print *, b2%i

   b2 = add ( add ( b1, b2 ), b2 )
   print *, b2%i
   
   b1 => b2
   b1 = add ( b1, b2 )
   
   print *, b1%i, b2%i

   contains

      type(base) function add(a, b)
         type(base), intent(in), pointer :: a
         type(base), intent(inout), allocatable  :: b

         pointer :: add

         print *, 'add'

         if ( .not. allocated  ( b ) ) allocate ( b, source = base(1000) )

         allocate ( add , source = a + b )

      end function

end program

type(base) function badd ( a, b )
   use m, only: base
   class(base), intent(in) :: a
   type(base), intent(in) :: b

   badd%i = a%i + b%i

   print *, 'badd'

end function
