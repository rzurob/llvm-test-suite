!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with assignment
!*
!*  DESCRIPTION                : assignment: named-constant (parameter) should still invoke the generic tb procedures
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
      character(3) :: c = 'xxx'
      contains
         procedure :: ab
         generic :: assignment(=) => ab
   end type


   type, extends( base ) :: child
      contains
         procedure :: ab => c
         generic :: assignment(=) => ab !<- specifying the same binding name twice
   end type

   contains

   subroutine ab ( a, b )
      class(base), intent(out) :: a
      class(base), intent(in) :: b
      a%c = b%c
   end subroutine

   subroutine c ( a, b )
      class(child), intent(out) :: a
      class(base), intent(in) :: b
      a%c = b%c(3:3) // b%c(2:2) // b%c(1:1)
   end subroutine


end module

program genericAssignmentParameter001
   use m

   type(base) :: b1 = base('IBM')
   type(base), parameter  :: b2 = base('FTN')
   type(child) :: c1 = child('xlf')
   type(child), parameter :: c2 = child('ghi')

   class(base), allocatable :: b3
   allocate ( b3 )

   b1 = b2
   print *, b1

   c1 = b2
   print *, c1

   c1 = c2
   print *, c1

   b3 = c2
   print *, b3%c

   deallocate ( b3 )

   allocate ( child :: b3 )
   b3 = c2

   print *, b3%c

end program
