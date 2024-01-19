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
!*                                      with elemental subroutine defined in base type
!*                                      child type has an overridding binding
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
      contains
         procedure :: bassgn
         generic :: assignment(=) => bassgn
   end type

   type, extends(base) :: child
      integer :: j
      contains
         procedure :: bassgn => cassgn
   end type

   type container
      class(base), allocatable :: b1(:)
      class(child), allocatable :: c1(:)
   end type

   contains

      elemental subroutine bassgn ( a, b )
         class(base), intent(out) :: a
         class(base), intent(in)  :: b

         a%i = b%i + 1

      end subroutine

      elemental subroutine cassgn ( a, b )
         class(child), intent(out) :: a
         class(base), intent(in)  :: b

         a%i = b%i + 1
         select type ( b )
            type is ( child )
               a%j = b%j + 1
         end select

      end subroutine

end module

program genericAssignmentDtIntrinAssgn028
   use m

   type(container) :: c1, c2, c3
   pointer :: c2
   allocatable :: c3

   allocate ( c2, c3 )

   c1 = container( (/ base(1), base(2), base(3) /), (/ child(4,5), child(6,7), child(8,9) /) )
   print *, c1%b1%i, c1%c1%i, c1%c1%j, 'bounds', lbound(c1%b1), ubound(c1%b1), lbound(c1%c1), ubound(c1%c1)

   c2 = c1
   print *, c2%b1%i, c2%c1%i, c2%c1%j, 'bounds', lbound(c2%b1), ubound(c2%b1), lbound(c2%c1), ubound(c2%c1)

   c3 = c2
   print *, c3%b1%i, c3%c1%i, c3%c1%j, 'bounds', lbound(c3%b1), ubound(c3%b1), lbound(c3%c1), ubound(c3%c1)

   deallocate ( c2%b1, c2%c1 )
   allocate ( c2%b1(2:5), source = (/ child(1,2), child(3,4), child(5,6), child(7,8) /) )
   allocate ( c2%c1(9:11), source = (/ child(11,12), child(13,14), child(15,16)/) )

   c3 = c2
   c1 = c3

   select type ( g => c1%b1 )
      type is ( child )
         select type ( h => c1%c1 )
            type is ( child )
               print *, g, h, 'bounds:', lbound(g), ubound(g), lbound(c1%c1), ubound(c1%c1)
         end select
   end select

   select type ( g => c2%b1 )
      type is ( child )
         select type ( h => c2%c1 )
            type is ( child )
               print *, g, h, 'bounds:', lbound(g), ubound(g), lbound(c2%c1), ubound(c2%c1)
         end select
   end select

   select type ( g => c3%b1 )
      type is ( child )
         select type ( h => c3%c1 )
            type is ( child )
               print *, g, h, 'bounds:', lbound(g), ubound(g), lbound(c3%c1), ubound(c3%c1)
         end select
   end select

   deallocate ( c3%b1, c3%c1 )
   allocate ( c3%b1(20:29)  , source = (/ ( child(j,j+1), j = 12,30,2 ) /) )
   select type ( g => c3%b1(20:29) )
      type is ( child )
         allocate ( c3%c1(-29:-20), source = g )
   end select

   c2 = c3
   c1 = c2

   select type ( g => c1%b1 )
      type is ( child )
         select type ( h => c1%c1 )
            type is ( child )
               print *, g, h, 'bounds:', lbound(g), ubound(g), lbound(c1%c1), ubound(c1%c1)
         end select
   end select

   select type ( g => c2%b1 )
      type is ( child )
         select type ( h => c2%c1 )
            type is ( child )
               print *, g, h, 'bounds:', lbound(g), ubound(g), lbound(c2%c1), ubound(c2%c1)
         end select
   end select

   select type ( g => c3%b1 )
      type is ( child )
         select type ( h => c3%c1 )
            type is ( child )
               print *, g, h, 'bounds:', lbound(g), ubound(g), lbound(c3%c1), ubound(c3%c1)
         end select
   end select

end program
