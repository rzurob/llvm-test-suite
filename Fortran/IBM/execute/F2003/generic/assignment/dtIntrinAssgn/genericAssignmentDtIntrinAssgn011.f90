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
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                      - try a derived type containing an derived type scalar and array
!*                                        with elemental UD assignment in generic tb
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
      integer :: i
      integer :: j(3)
      contains
         procedure :: bassgn
         generic :: assignment(=) => bassgn
   end type

   type container
      type(base) :: b1       ! scalar
      type(base) :: b2(2:6)  ! array
   end type

   contains

      elemental subroutine bassgn ( a, b )
         class(base), intent(out) :: a
         class(base), intent(in) :: b

         a%i = b%i + 1
         a%j = b%j + 1

      end subroutine

end module

program genericAssignmentDtIntrinAssgn011
   use m

   type(container), pointer ::  c1
   type(container) ::  c2, c3

   allocatable :: c2

   allocate ( c1, source = container ( base(1,(/2,3,4/)), (/ ( base(i,(/i+1,i+2,i+3/)), i = 5,21,4 ) /) ) )
   allocate ( c2, source = container ( base(2,(/3,4,5/)), (/ ( base(i,(/i+1,i+2,i+3/)), i = 6,22,4 ) /) ) )
   print *, c1
   print *, c2

   c3 = c1
   print *, c3

   c1 = c2
   print *, c1

   c1%b2 = c2%b2(6:2:-1)
   print *, c1

end program
