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
!*  DESCRIPTION                : assignment: with some deferred binding with different ranks
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
         procedure(dintf), deferred, pass :: assgnscalar
         procedure(dintf1d), deferred, pass :: assgn1d
         procedure(dintf2d), deferred, pass :: assgn2d
         procedure(dintf3d), deferred, pass :: assgn3d
         generic :: assignment(=) => assgnscalar, assgn1d, assgn2d, assgn3d
   end type

   type, extends(base) :: child
      contains
         procedure, pass :: assgnscalar
         procedure, pass :: assgn1d
         procedure, pass :: assgn2d
         procedure, pass :: assgn3d
   end type

   interface
      subroutine dintf (a, b)
         import base
         class(base), intent(inout) :: a
         class(base), intent(in) :: b
      end subroutine
   end interface

   interface
      subroutine dintf1d (a, b)
         import base
         class(base), intent(inout) :: a
         class(base), intent(in) :: b(:)
      end subroutine
   end interface

   interface
      subroutine dintf2d (a, b)
         import base
         class(base), intent(inout) :: a
         class(base), intent(in) :: b(:,:)
      end subroutine
   end interface

   interface
      subroutine dintf3d (a, b)
         import base
         class(base), intent(inout) :: a
         class(base), intent(in) :: b(:,:,:)
      end subroutine
   end interface

   contains

   subroutine assgnscalar ( a, b)
      class(child), intent(inout) :: a
      class(base), intent(in) :: b

      a%i = b%i

      print *, 'assgnscalar'

   end subroutine

   subroutine assgn1d ( a, b)
      class(child), intent(inout) :: a
      class(base), intent(in) :: b(:)

      a%i = b(1)%i

      print *, 'assgn1d'

   end subroutine

   subroutine assgn2d ( a, b)
      class(child), intent(inout) :: a
      class(base), intent(in) :: b(:,:)

      a%i = b(1,1)%i

      print *, 'assgn2d'

   end subroutine

   subroutine assgn3d ( a, b)
      class(child), intent(inout) :: a
      class(base), intent(in) :: b(:,:,:)

      a%i = b(1,1,1)%i

      print *, 'assgn3d'

   end subroutine

end module

program genericAssignmentDeferred003
   use m

   class(base), allocatable :: b0
   class(base), pointer :: b1(:), b2(:,:), b3(:,:,:)

   type(child) :: c0, c1(1), c2(1,1), c3(1,1,1)

   allocate ( child :: b0,  b1(1), b2(1,1), b3(1,1,1) )

   b0%i = 50
   b1(1)%i = 100
   b2(1,1)%i = 200
   b3(1,1,1)%i = 300

   b0 = b0
   if ( b0%i /= 50 ) error stop 1_4

   b0 = b1
   if ( b0%i /= 100 ) error stop 2_4

   b0 = b2
   if ( b0%i /= 200 ) error stop 3_4

   b0 = b3
   if ( b0%i /= 300 ) error stop 4_4

   b0 = reshape ( b1, shape = (/1,1,1/))
   if ( b0%i /= 100 ) error stop 5_4

   b0 = reshape ( b3, shape = (/1,1/))
   if ( b0%i /= 300 ) error stop 6_4

   b0 = b2(1,1)
   if ( b0%i /= 200 ) error stop 7_4

   c0%i = 50

   c0 = c0
   if ( c0%i /= 50 ) error stop 8_4

   c0 = b1
   if ( c0%i /= 100 ) error stop 9_4

   c0 = b2
   if ( c0%i /= 200 ) error stop 10_4

   c0 = b3
   if ( c0%i /= 300 ) error stop 11_4

   c0 = reshape ( b1, shape = (/1,1,1/))
   if ( c0%i /= 100 ) error stop 12_4

   c0 = reshape ( b3, shape = (/1,1/))
   if ( c0%i /= 300 ) error stop 13_4

   c0 = b2(1,1)
   if ( c0%i /= 200 ) error stop 14_4

end program
