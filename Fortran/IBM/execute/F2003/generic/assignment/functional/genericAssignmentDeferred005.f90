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
!*  DESCRIPTION                : assignment: deferred elemental binding being generic assignment
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

   type, abstract :: base
      integer(4) :: i = -999
      contains
         procedure(itf), deferred, pass :: bagnmt
         generic :: assignment(=) => bagnmt
   end type

   interface
      elemental subroutine itf ( a, b )
         import base
         class(base), intent(out) :: a
         class(base), intent(in)  :: b
      end subroutine
   end interface

end module

module m1
   use m

   type, extends(base) :: child
      integer(4) :: j = -999
      contains
         procedure, pass :: bagnmt => cagnmt
   end type

   contains

      elemental subroutine cagnmt ( a, b )
         class(child), intent(out) :: a
         class(base), intent(in)  :: b

         a%i = b%i + 1
         select type ( b )
            type is ( child )
               a%j = b%j+1
         end select

      end subroutine


end module

program genericAssignmentDeferred005
   use m1

   class(base), allocatable  :: b1, b2(:), b3(:,:)
   class(child), allocatable :: c1, c2(:), c3(:,:)

   allocate ( child :: b1, b2(3), b3(2,2), c1, c2(3), c3(2,2) )

   c1%i = 100
   c1%j = 1000

   c2 = c1

   c3 = reshape ( source = (/ c2(1:2), child(200, 2000), child ( 300, 3000) /), shape = (/2,2/) )

   print *, c2%i
   print *, c2%j

   print *, c3%i
   print *, c3%j

   c2(1:2) = c3(2,1:2)
   print *, c2%i
   print *, c2%j

   b1 = c1

   select type ( b1 )
      type is ( child )
         print *, b1%i
         print *, b1%j
   end select
   
   b2 = c1

   select type ( b2 )
      type is ( child )
         print *, b2%i
         print *, b2%j
   end select
   
   b2 = c3(2, (/1,2,1 /) )

   select type ( b2 )
      type is ( child )
         print *, b2%i
         print *, b2%j
   end select
   
   b3 = c3
   
   select type ( b3 )
      type is ( child )
         print *, b3%i
         print *, b3%j
   end select

end program
