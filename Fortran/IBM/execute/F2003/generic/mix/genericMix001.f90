!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : Mix generic type bounds
!*
!*  DESCRIPTION                : mix add, subtract, and assignment in a type
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module myint

   type int
      integer :: i
      contains

         procedure, pass :: add
         procedure, pass :: subtract
         procedure, pass :: assignment

         generic :: operator(+) => add
         generic :: operator(-) => subtract
         generic :: assignment(=) => assignment

   end type

   contains

      subroutine assignment(a,b)
         class(int), intent(out) :: a
         class(int), intent(in)  :: b

         print *, 'assignment'
         a%i = b%i

      end subroutine

      type(int) function add ( a, b)
         class(int), intent(in) :: a
         class(int), intent(in) :: b

         print *, 'add'
         add%i = a%i + b%i

      end function

      type(int) function subtract ( a, b)
         class(int), intent(in) :: a
         class(int), intent(in) :: b

         print *, 'sub'
         subtract = a + int(-1*b%i)

      end function

end module

program genericMix001
   use myint

   type(int) :: i
   type(int), allocatable :: j
   class(int), pointer :: k

   allocate ( k , j )

   i = int(10) + int(5)
   j = i + int(15) - int(10)
   k = i + j - i - j

   print *, i%i
   print *, j%i
   print *, k%i

end program
