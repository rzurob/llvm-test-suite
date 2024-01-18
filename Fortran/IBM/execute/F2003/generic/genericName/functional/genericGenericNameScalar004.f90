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
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : generic-name: scalar derived type calling
!*                                             generic bindings of two different types pointing to the same
!*                                             procedure
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

   type base1
      integer :: i
      contains
         procedure, pass :: call => twotypesb1
         generic :: add => call
   end type

   type base2
      integer :: i
      contains
         procedure, pass(b) :: call => twotypesb2
         generic :: add => call
   end type

   contains

      integer function twotypesb1 ( a, b )
         class(base1), intent(in) :: a
         class(base2), intent(in) :: b

         twotypesb1 = a%i + b%i

         print *, 'twotypesb1'

      end function

      integer function twotypesb2 ( a, b )
         class(base1), intent(in) :: a
         class(base2), intent(in) :: b

         twotypesb2 = a%i + b%i

         print *, 'twotypesb2'

      end function

end module

program genericGenericNameScalar004
   use m

   type(base1) :: b1
   type(base1), pointer :: b2

   type(base2) :: b3
   type(base2), allocatable :: b4

   integer :: i

   allocate ( b2, b4 )

   b1 = base1(100)
   b2 = base1(200)
   b3 = base2(300)
   b4 = base2(400)

   i = b1%call(b3)
   print *,i

   i = b2%call(b4)
   print *,i

   i = b3%call(b2)
   print *,i

   i = b4%call(b1)
   print *,i

end program
