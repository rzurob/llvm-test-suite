!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Binary Operator: Implicit statement with polymorphic types
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
      integer :: x = 2
      integer :: y = 4
      contains
         procedure, pass :: mybmul
         procedure, pass :: mybdiv
         generic :: operator(*) => mybmul
         generic :: operator(/) => mybdiv
   end type

   type, extends(base) :: child
   end type

   contains

   function mybmul ( a, b )
      class(base), intent(in) :: a
      class(base), intent(in)  :: b

      type(base) :: mybmul

      mybmul%x = a%x * b%x
      mybmul%y = a%y * b%y

      print *, 'mybmul'

   end function

   function mybdiv ( a, b )
      class(base), intent(in) :: a
      class(base), intent(in)  :: b

      type(base) :: mybdiv

      mybdiv%x = a%x / b%x
      mybdiv%y = a%y / b%y

      print *, 'mybdiv'

   end function

end module

program genericOperatorImplicit002
   use m

   implicit class(base)  (a-i)
   implicit class(child) (j-z)

   allocatable :: a,b,c,x,y,d

   allocate ( b,c,y,x )

   allocate ( a, source = b*c )
   allocate ( d, source = x*y )

   print *, a%x, a%y
   print *, d%x, d%y

   deallocate ( a,d )

   allocate ( a, source = b/c )
   allocate ( d, source = x/y )

   print *, a%x, a%y
   print *, d%x, d%y

end program
