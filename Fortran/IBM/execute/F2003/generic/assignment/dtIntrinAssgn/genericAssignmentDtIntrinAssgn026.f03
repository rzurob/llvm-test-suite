!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                 - for allocatable component
!*                                    - if it's an array,
!*                                      it's allocated with the same bound.
!*                                      try without generic tb with polymorphic array components
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
   end type

   type, extends(base) :: child
      integer :: j
   end type

   type, extends(child) :: gen3
      integer :: k
   end type

   type container
      class(base), allocatable  :: b1(:)
      class(child), allocatable :: c1(:)
   end type

   interface assignment(=)
      module procedure baseassignment
   end interface

   contains

      subroutine baseassignment(a, b)
         class(base), intent(out) :: a(:)
         class(base), intent(in) :: b(:)

         error stop 1_4

      end subroutine

end module

program genericAssignmentDtIntrinAssgn026
   use m

   type(container) :: c1, c2, c3
   pointer :: c2
   allocatable :: c3

   allocate ( c2, c3 )
   c1 = container ( (/ base(1), base(2), base(3) /) , (/ child(4,5), child(6,7) /) )
   c2 = c1
   c3 = c2

   print *, c1%b1%i, c1%c1%i, c1%c1%j
   print *, 'b1 bounds:', lbound(c1%b1), ubound(c1%b1),  'c1 bounds:', lbound(c1%c1), ubound(c1%c1)
   print *, c2%b1%i, c2%c1%i, c2%c1%j
   print *, 'b1 bounds:', lbound(c2%b1), ubound(c2%b1),  'c1 bounds:', lbound(c2%c1), ubound(c2%c1)
   print *, c3%b1%i, c3%c1%i, c3%c1%j
   print *, 'b1 bounds:', lbound(c3%b1), ubound(c3%b1),  'c1 bounds:', lbound(c3%c1), ubound(c3%c1)

   deallocate ( c2%b1, c2%c1 )
   allocate ( c2%b1(-2:2), source = (/ child(4,5), child(6,7), child(8,9), child(10,11), child(12,13) /) )
   allocate ( c2%c1(-50:-48), source = (/ gen3(6,7,8), gen3(8,9,10), gen3(10,11,12) /) )

   c1 = c2
   c3 = c1

   select type ( g => c1%b1 )
      type is ( child )
         print *, g, 'b1 bounds:', lbound(g), ubound(g)
   end select

   select type ( g => c1%c1 )
      type is ( gen3 )
         print *, g, 'c1 bounds:', lbound(g), ubound(g)
   end select

   select type ( g => c2%b1 )
      type is ( child )
         print *, g, 'b1 bounds:', lbound(g), ubound(g)
   end select

   select type ( g => c2%c1 )
      type is ( gen3 )
         print *, g, 'c1 bounds:', lbound(g), ubound(g)
   end select

   select type ( g => c3%b1 )
      type is ( child )
         print *, g, 'b1 bounds:', lbound(g), ubound(g)
   end select

   select type ( g => c3%c1 )
      type is ( gen3 )
         print *, g, 'c1 bounds:', lbound(g), ubound(g)
   end select

   deallocate ( c3%b1 , c3%c1 )

   allocate ( c3%b1(-1000000000_8: -999999998_8 ), source = (/ gen3(4,5,6), gen3(6,7,8), gen3(8,9,10) /) )
   allocate ( gen3 :: c3%c1(10:9) )

   c1 = c3
   c2 = c1

   select type ( g => c1%b1 )
      type is ( gen3 )
         print *, g, 'b1 bounds:', lbound(g), ubound(g)
   end select

   select type ( g => c1%c1 )
      type is ( gen3 )
         print *, g, 'c1 bounds:', lbound(g), ubound(g)
   end select

   select type ( g => c2%b1 )
      type is ( gen3 )
         print *, g, 'b1 bounds:', lbound(g), ubound(g)
   end select

   select type ( g => c2%c1 )
      type is ( gen3 )
         print *, g, 'c1 bounds:', lbound(g), ubound(g)
   end select

   select type ( g => c3%b1 )
      type is ( gen3 )
         print *, g, 'b1 bounds:', lbound(g), ubound(g)
   end select

   select type ( g => c3%c1 )
      type is ( gen3 )
         print *, g, 'c1 bounds:', lbound(g), ubound(g)
   end select

end program
