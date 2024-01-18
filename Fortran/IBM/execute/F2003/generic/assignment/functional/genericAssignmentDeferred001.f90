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
!*  DESCRIPTION                : assignment: with deferred binding
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

   type, abstract :: incompletept
      integer(4) :: x = -999
      contains
         procedure(dintf), deferred, pass :: assgn
         generic :: assignment(=) => assgn
   end type

   type, extends(incompletept) :: pt
      integer(4) :: y = -999
      contains
         procedure, pass :: assgn
   end type

   interface
      subroutine dintf (a, b)
         import incompletept
         class(incompletept), intent(out) :: a
         class(incompletept), intent(in) :: b
      end subroutine
   end interface

   contains

   subroutine assgn ( a, b)
      class(pt), intent(out) :: a
      class(incompletept), intent(in) :: b

      a%x = b%x
      select type ( b )
         type is ( pt )
            a%y = b%y
      end select

      print *, 'assgn'

   end subroutine

end module

program genericAssignmentDeferred001
   use m

   class(incompletept), pointer :: p1
   type(pt), target :: p2

   class(pt), allocatable :: p3

   allocate ( p1, source = pt(3,6) )
   p2 = pt ( 4, 8 )

   allocate ( p3 )

   p1 = p2
   p3 = p1

   select type ( p1 )
      type is ( pt )
         print *, p1
   end select

   print *, p2

   select type ( p3 )
      type is ( pt )
         print *, p3
   end select

   p1 = pt ( 10, 20 )

   select type ( p1 )
      type is ( pt )
         print *, p1
   end select

end program
