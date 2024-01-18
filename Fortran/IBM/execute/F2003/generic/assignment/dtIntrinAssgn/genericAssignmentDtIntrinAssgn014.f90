!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                      - container type contains a derived type
!*                                        component that has UD assignment of elemental and different ranks
!*                                        but it should only resolve to the elemental type bound
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

   type inner
      integer i
      contains
         procedure :: itoi0
         generic :: assignment(=) => itoi0
   end type

   type container
      type(inner) :: inn0
      type(inner) :: inn1(3)
      type(inner) :: inn2(2,2)
      type(inner) :: inn3(2,2,2)
   end type

   contains

      elemental subroutine itoi0( a, b )
         class(inner), intent(out) :: a
         class(inner), intent(in) :: b

         a%i = b%i + 1

      end subroutine

end module

module n
   use m, only: container, inner

   interface assignment(=)
      module procedure itoi1
      module procedure itoi2
      subroutine itoi3( a, b )
         import inner
         class(inner), intent(out) :: a(:,:,:)
         class(inner), intent(in) :: b(:,:,:)
      end subroutine
   end interface

   contains

       subroutine itoi1( a, b )
         class(inner), intent(out) :: a(:)
         class(inner), intent(in) :: b(:)

         a%i = b%i

      end subroutine

      subroutine itoi2( a, b )
         class(inner), intent(out) :: a(:,:)
         class(inner), intent(in) :: b(:,:)

         a%i = b%i

      end subroutine

 end module

subroutine itoi3( a, b )
   use m, only: inner
   class(inner), intent(out) :: a(:,:,:)
   class(inner), intent(in) :: b(:,:,:)

   a%i = b%i

end subroutine

program genericAssignmentDtIntrinAssgn014
   use n

   type(container) :: c1, c2

   allocatable :: c2

   c1 = container( inner(1), (/ inner(2), inner(3), inner(4) /), reshape( source = (/ inner(5), inner(6), inner(7), inner(8)  /), shape =(/2,2/) ), &
        & reshape( source = (/ inner(9), inner(10), inner(11), inner(12), inner(13), inner(14), inner(15), inner(16)  /), shape =(/2,2,2/) ) )

   print *, c1
   allocate ( c2 )

   c2 = c1
   print *, c2

   c1 = c2
   print *, c1

   c2%inn0 = c1%inn0
   c2%inn1 = c1%inn1
   c2%inn2 = c1%inn2
   c2%inn3 = c1%inn3

   print *, c2

end program
