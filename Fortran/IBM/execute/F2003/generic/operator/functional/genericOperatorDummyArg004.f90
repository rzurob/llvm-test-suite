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
!*  SECONDARY FUNCTIONS TESTED : with Operator(*)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : operator: non-poly (explicit size) array dummy arguments being the operand
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
         procedure, pass :: bmul
         generic :: operator(*) => bmul
   end type

   interface
      integer function bmul ( a, b )
         import base
         class(base), intent(in) :: a
         type(base), intent(in) :: b(3)
         dimension :: bmul(3)
      end function
   end interface

end module

program genericOperatorDummyArg004
   use m

   type(base) :: b1, b2(5)
   type(base), allocatable :: b3(:)
   
   integer :: i(3)

   b1 = base(100)
   print *, b1%i

   b2 = base(200)
   print *, b2%i

   i= mul( b1, b2 )
   print *, i

   allocate ( b3(3), source = (/ base(1), base(2), base(3) /) )

   i= mul (b1, b3)
   print *, i

   i= mul1( b1, b2(5:1:-2))
    print *, i

   i= mul1 (b1, b3((/2,1,2/)))
   print *, i

   contains

      integer function mul(a, b)
         type(base), intent(inout) :: a
         type(base), intent(in)  :: b(5)
         
         dimension :: mul(3)

         print *, 'mul'
         mul = a * b

      end function

      integer function mul1(a, b)
         type(base), intent(inout) :: a
         type(base), intent(in)  :: b(2:4)

         dimension :: mul1(3)

         print *, 'mul1'
         mul1= a * b(2:4)

      end function

end program

integer function bmul ( a, b )
   use m, only: base
   class(base), intent(in) :: a
   type(base), intent(in) :: b(3)

   dimension :: bmul(3)
   
   do k = 1,3
      bmul(k) =  a%i * b(k)%i
   end do

   print *, 'bmul'

end function
