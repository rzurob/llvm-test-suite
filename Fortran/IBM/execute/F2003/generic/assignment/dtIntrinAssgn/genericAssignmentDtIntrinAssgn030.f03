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
!*                                      with multi-dimensional array
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

module m

   type base
      integer :: i
      contains
         procedure :: bassgn
         generic :: assignment(=) => bassgn
   end type

   type, extends(base) :: child
      integer :: j
   end type

   type container
      class(base), allocatable :: b1(:,:)
      class(child), allocatable :: c1(:,:,:)
   end type

   interface assignment(=)
      module procedure arraytoarray
   end interface

   contains

      subroutine arraytoarray ( a, b )
         class(base), intent(out) :: a(:,:)
         class(base), intent(in)  :: b(:,:)

         stop 100

      end subroutine

      elemental subroutine bassgn ( a, b )
         class(base), intent(out) :: a
         class(base), intent(in)   :: b

         a%i = b%i + 1

         select type ( a )
            type is ( child )
               select type ( b )
                  type is ( child )
                     a%j = b%j + 1
               end select
         end select

      end subroutine

end module

program genericAssignmentDtIntrinAssgn030
   use m

   type(container) :: c1, c2, c3
   pointer :: c2
   allocatable :: c3

   allocate ( c2, c3 )

   c1 = container( reshape ( source = (/ base(1), base(2), base(3), base(4) /), shape = (/2,2/) ), &
      &            reshape ( source = (/ child(0,1), child(2,3), child(4,5), child(6,7),  child(8,9), child(10,11), child(12,13), child(14,15) /), shape = (/2,2,2/) ) )

   print *, c1%b1%i, c1%c1%i, c1%c1%j, 'bounds:', lbound(c1%b1), ubound(c1%b1), lbound(c1%c1), ubound(c1%c1)

   c2 = c1
   c3 = c2

   print *, c2%b1%i, c2%c1%i, c2%c1%j, 'bounds:', lbound(c2%b1), ubound(c2%b1), lbound(c2%c1), ubound(c2%c1)
   print *, c3%b1%i, c3%c1%i, c3%c1%j, 'bounds:', lbound(c3%b1), ubound(c3%b1), lbound(c3%c1), ubound(c3%c1)

   deallocate ( c3%b1, c3%c1 )

   allocate ( c3%b1(4:5,8:9), source = reshape ( source = (/ child(0,1), child(2,3), child(4,5), child(6,7) /), shape = (/2,2/) ) )
   allocate ( c3%c1(-3:-2,100:101,10000:10001), source = reshape ( source = (/ child(10,11), child(12,13), child(14,15), child(16,17),  child(18,19), child(20,21), child(22,23), child(24,25)  /), shape = (/2,2,2/) ) )

   c1 = c3
   c2 = c1

   select type ( g => c1%b1 )
      type is ( child )
         print *, g%i, g%j, c1%c1%i, c1%c1%j, 'bounds:', lbound(g), ubound(c1%b1), lbound(c1%c1), ubound(c1%c1)
   end select

   select type ( g => c2%b1 )
      type is ( child )
         print *, g%i, g%j, c2%c1%i, c2%c1%j, 'bounds:', lbound(g), ubound(c2%b1), lbound(c2%c1), ubound(c2%c1)
   end select

   select type ( g => c3%b1 )
      type is ( child )
         print *, g%i, g%j, c3%c1%i, c3%c1%j, 'bounds:', lbound(g), ubound(c3%b1), lbound(c3%c1), ubound(c3%c1)
   end select

end program
