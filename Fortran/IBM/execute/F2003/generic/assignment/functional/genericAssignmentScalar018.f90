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
!*  DESCRIPTION                : assignment: polymorphic component that has generic assignment tb as well
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
      integer :: x = -999
      contains
         procedure, private :: innerassignment
         generic, private :: assignment(=) => innerassignment
   end type

   type, extends(inner) :: innerchild
      integer :: y = -999
      contains
         procedure, private :: innerassignment => innerchildassignment
   end type

   type base
      class(inner), allocatable :: i
      integer :: j
      contains
         generic :: assignment(=) => assignment
         procedure :: assignment
   end type

   contains

   subroutine innerassignment ( a, b)
      class(inner), intent(out) :: a
      class(inner), intent(in) :: b

      print *, 'innerassgn'
      a%x = b%x

   end subroutine

   subroutine innerchildassignment ( a, b)
      class(innerchild), intent(out) :: a
      class(inner), intent(in) :: b

      print *, 'innerchildassgn'
      a%inner = inner( b%x )

      select type ( b )
         type is ( innerchild )
            a%y = b%y
      end select

   end subroutine

   subroutine assignment ( a, b)
      class(base), intent(out) :: a
      class(base), intent(in) :: b

      print *, 'assign'
      if ( .not. allocated (a%i) ) allocate ( a%i , source = b%i )

      a%i = b%i
      a%j = b%j

   end subroutine

end module

program genericAssignmentScalar018
   use m

   type(base) :: b1
   type(base), allocatable :: b2

   b1 = base ( inner(50), 100 )

   print *, b1%i%x
   print *, b1%j

   allocate (  b2 )
   b2 = b1
   print *, b2%i%x
   print *, b2%j

   b1 = base ( innerchild(500, 1000) , 20000 )

   select type ( g => b1%i )
      type is (innerchild)
         print *, g%x, g%y
         print *, b1%j
   end select

   b2 = b1
   select type ( g => b1%i )
      type is (innerchild)
         print *, g%x, g%y
         print *, b1%j
   end select

end program