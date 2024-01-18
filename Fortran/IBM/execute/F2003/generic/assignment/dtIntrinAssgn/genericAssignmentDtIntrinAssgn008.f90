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
!*                                  - A derived-type intrinsic assignment is performed as if each component of variable
!*                                    were assigned from the corresponding component of expr using pointer
!*                                    assignment for each pointer component
!*                                      - in the same derived type as component, pointer, allocatable in a containing
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
      integer :: x
      contains
         procedure, pass :: baseassignment
         generic :: assignment(=) => baseassignment
   end type


   type container
      type(base) :: b1
      type(base), pointer :: b2
      type(base), allocatable :: b3
   end type

   type, extends(container) :: child_container
      type(base) :: bb1
      type(base), pointer :: bb2
      type(base), allocatable :: bb3
   end type

   contains

      subroutine baseassignment ( a, b )
         class(base), intent(out) :: a
         class(base), intent(in)  :: b

         a%x = b%x + 1

         print *, 'baseassignment'

      end subroutine

end module

program genericAssignmentDtIntrinAssgn008
   use m

   type(container) :: c1
   type(child_container), pointer :: c2
   class(container), allocatable :: c3

   type(base), pointer :: b1
   allocate ( b1,source = base(200) )

   print *, 'set c1'
   c1 = container( base(100), b1, base(300) )
   print *, 'allocate c2'
   allocate ( c2 )
   c2 = child_container( base(1000), b1, base(2000), base(3000), b1, base(4000) )
   print *, 'allocate c3'
   allocate ( c3, source = container( base(-999), null(), base(-999) ) )

   select type ( c3 )
      type is ( container )
         print *, 'set c3'
         c3 = c1
         print *, c3%b1, c3%b2, c3%b3, associated ( c3%b2, c1%b2 ), associated ( c3%b2, b1 )
   end select

   deallocate ( c3 )
   allocate ( child_container :: c3 )

   select type ( c3 )
      type is ( child_container )
         print *, 'set c3'
         c3 = c2
         print *, c3%b1, c3%b2, c3%b3, c3%bb1, c3%bb2, c3%bb3, associated ( c3%b2, c2%b2 ), associated ( c3%b2, b1 )
   end select

end program
