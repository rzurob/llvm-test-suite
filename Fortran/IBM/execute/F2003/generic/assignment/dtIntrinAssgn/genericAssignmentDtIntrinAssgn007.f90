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
!*                                      - try derived type nonpointer nonallocatable component containing pointer components
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

   type innercomp1
      integer, pointer :: i
      contains
         generic :: assignment(=) => c1assgn
         procedure, pass :: c1assgn
   end type

   type innercomp2
      integer, pointer :: j
   end type


   type comp1
      integer :: i
      type(innercomp1), pointer :: ic1
   end type

   type comp2
      integer :: j
      type(innercomp2) :: ic2
   end type

   type base
      type(comp1) :: c1
      type(comp2) :: c2
      type(innercomp1), pointer :: ic1
      type(innercomp2) :: ic2
   end type

   contains

      subroutine c1assgn ( a, b )
         class(innercomp1), intent(out) :: a
         class(innercomp1), intent(in) :: b

         error stop 1_4

      end subroutine

end module

program genericAssignmentDtIntrinAssgn007
   use m

   type(base) :: b1
   class(base), pointer :: b2
   class(base), allocatable :: b3

   type(innercomp1), pointer :: ic1
   integer, pointer :: i1

   allocate ( i1, source = 30 )
   allocate ( ic1 , source = innercomp1(i1) )

   allocate ( b2, b3 )

   b1 = base( comp1( 10, ic1 ), comp2(i1, innercomp2(i1)), ic1, innercomp2(i1) )
   print *, b1%c1%i, b1%c1%ic1%i, b1%c2%j, b1%c2%ic2%j, b1%ic1%i, b1%ic2%j

   select type ( b2 )
      type is ( base )
         b2 = b1
         print *, b2%c1%i, b2%c1%ic1%i, b2%c2%j, b2%c2%ic2%j, b2%ic1%i, b2%ic2%j

         if (  ( .not. associated(b1%c1%ic1%i, b2%c1%ic1%i ) ) .or. &
         &     ( .not. associated(b1%c2%ic2%j, b2%c2%ic2%j ) ) .or. &
         &     ( .not. associated(b1%ic1%i, b2%ic1%i ) )       .or. &
         &     ( .not. associated(b1%ic2%j, b2%ic2%j ) ) )     error stop 1_4

   end select

   select type ( b3 )
      type is ( base )
         b3 = b1
         print *, b3%c1%i, b3%c1%ic1%i, b3%c2%j, b3%c2%ic2%j, b3%ic1%i, b3%ic2%j

         if (  ( .not. associated(b1%c1%ic1%i, b3%c1%ic1%i ) ) .or. &
         &     ( .not. associated(b1%c2%ic2%j, b3%c2%ic2%j ) ) .or. &
         &     ( .not. associated(b1%ic1%i, b3%ic1%i ) )       .or. &
         &     ( .not. associated(b1%ic2%j, b3%ic2%j ) ) )     error stop 2_4

         if (  ( .not. associated(b2%c1%ic1%i, b3%c1%ic1%i ) ) .or. &
         &     ( .not. associated(b2%c2%ic2%j, b3%c2%ic2%j ) ) .or. &
         &     ( .not. associated(b2%ic1%i, b3%ic1%i ) )       .or. &
         &     ( .not. associated(b2%ic2%j, b3%ic2%j ) ) )     error stop 3_4

   end select

end program
