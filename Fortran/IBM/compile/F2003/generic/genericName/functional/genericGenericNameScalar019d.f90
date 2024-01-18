!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : generic-name: calling generic TB with array in which there is no correct TB to be resolved
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

   type :: base
      integer(4) :: i
      contains
         procedure, pass :: b_b
         generic :: print => b_b
   end type

   contains

      subroutine b_b( a, b )
         class(base), intent(in) :: a, b
         print *, a%i, b%i
      end subroutine

end module

program genericGenericNameScalar019d
   use m

   class(base), allocatable :: b1, b2(:)

   allocate ( b1, source = base(100) )
   allocate ( b2(5), source = (/ ( base(100*i), i = 2, 6 ) /) )

   call b2%print(b1)

end program
