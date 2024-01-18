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
!*                                 - for allocatable component
!*                                    - if it's an array,
!*                                      it's allocated with the same bound.
!*                                      with elemental subroutine defined in base type
!*                                      and child type define another generic assignment
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
      contains
         procedure :: cassgn
         generic :: assignment(=) => cassgn
   end type

   type container
      class(base), allocatable :: b1(:)
      class(child), allocatable :: c1(:)
   end type

   interface assignment(=)
      module procedure arraytoarray
   end interface

   contains

      subroutine arraytoarray ( a, b )
         class(base), intent(out) :: a(:)
         class(base), intent(in)  :: b(:)

         stop 100

      end subroutine

      elemental subroutine bassgn ( a, b )
         class(base), intent(out) :: a
         type(base), intent(in)   :: b

         a%i = b%i + 1

         select type ( a )
            type is ( child )
               a%j = b%i + 2
         end select

      end subroutine

      elemental subroutine cassgn ( a, b )
         class(child), intent(out) :: a
         type(child), intent(in)   :: b

         a%i = b%i + 2
         a%j = b%j + 2

      end subroutine

end module

program genericAssignmentDtIntrinAssgn029
   use m

   type(container) :: c1, c2, c3
   pointer :: c2
   allocatable :: c3

   allocate ( c2, c3 )

   allocate ( c1%b1(-9:-7), c1%c1(-100:-98) )

   c1 = container( (/ base(1), base(2), base(3) /), (/ child(4,5), child(6,7), child(8,9) /) )    !<- this assignment should deallocate c1%b1 and c1%c1 first
   print *, c1%b1%i, c1%c1%i, c1%c1%j, 'bounds', lbound(c1%b1), ubound(c1%b1), lbound(c1%c1), ubound(c1%c1)

   c2 = c1
   print *, c2%b1%i, c2%c1%i, c2%c1%j, 'bounds', lbound(c2%b1), ubound(c2%b1), lbound(c2%c1), ubound(c2%c1)

   c3 = c2
   print *, c3%b1%i, c3%c1%i, c3%c1%j, 'bounds', lbound(c3%b1), ubound(c3%b1), lbound(c3%c1), ubound(c3%c1)

   deallocate ( c2%b1, c2%c1 )
   allocate ( c2%b1(-1:2), source = (/ child(1,2), child(3,4), child(5,6), child(7,8) /) )
   allocate ( c2%c1(11:13), source = (/ child(9,10), child(11,12), child(13,14)/) )

   c1 = c2
   c3 = c1

   select type ( g => c1%b1 )
      type is ( child )
         select type ( h => c1%c1 )
            type is ( child )
               print *, g%i, g%j, h, 'bounds:', lbound(g), ubound(c1%b1), lbound(h), ubound(c1%c1)
         end select
   end select

   select type ( g => c2%b1 )
      type is ( child )
         select type ( h => c2%c1 )
            type is ( child )
               print *, g%i, g%j, h, 'bounds:', lbound(g), ubound(g), lbound(c2%c1), ubound(c2%c1)
         end select
   end select

   select type ( g => c3%b1 )
      type is ( child )
         select type ( h => c3%c1 )
            type is ( child )
               print *, g%i, g%j, h, 'bounds:', lbound(g), ubound(c3%b1), lbound(h), ubound(c3%c1)
         end select
   end select

end program
