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
!*                                 - polymorphic container with allocatable array components
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
      class(base), allocatable :: b1(:)
   end type

   type, extends(container) :: c_container
      type(child), allocatable  :: c1(:,:)
   end type

   contains

      elemental subroutine bassgn ( a, b )
         class(base), intent(out) :: a
         class(base), intent(in)  :: b

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

program genericAssignmentDtIntrinAssgn035
   use m

   class(container), allocatable :: c1, c2
   class(c_container), allocatable :: c3

   allocate ( c1, c2, c3 )

   select type ( g => c1 )
      type is ( container )
         g = container((/ base(1), base(2), base(3) /))
         print *, g%b1%i

         select type ( c2 )
            type is ( container )
               c2 = c1
               print *, c2%b1%i
         end select

   end select

   deallocate ( c1, c2 )

   allocate ( c_container :: c2, c1 )

   select type ( c2 )
      type is ( c_container )

         c2 = c_container( (/ base(4), base(5), base(6) /) , reshape ( source = (/ child(7,8), child(9,10), child(11,12), child(13,14)  /), shape = (/2,2/) ) )
         print *, c2%b1%i, c2%c1%i, c2%c1%j

         select type ( c1 )
            type is ( c_container )
               c1 = c2
               print *, c1%b1%i, c1%c1%i, c1%c1%j
         end select

         select type ( c3 )
            type is ( c_container )
               c3 = c2
               print *, c3%b1%i, c3%c1%i, c3%c1%j
         end select

   end select

   deallocate ( c1, c2 )

   allocate ( c_container :: c2, c1 )

   select type ( c2 )
      type is ( c_container )

         c2 = c_container( (/ child(100,101), child(102,103), child(104,105) /) , reshape ( source = (/ child(106,107), child(108,109), child(110,111), child(112,113)  /), shape = (/2,2/) ) )
         select type ( h => c2%b1 )
            type is ( child )
               print *, h%i, h%j, c2%c1%i, c2%c1%j
         end select

         select type ( c1 )
            type is ( c_container )
               c1 = c2
               select type ( h => c1%b1 )
                  type is ( child )
                     print *, h%i, h%j,  c1%c1%i, c1%c1%j
               end select
         end select

         select type ( c3 )
            type is ( c_container )
               c3 = c2
               select type ( h => c3%b1 )
                  type is ( child )
                     print *, h%i, h%j, c3%c1%i, c3%c1%j
               end select
         end select

   end select

end program
