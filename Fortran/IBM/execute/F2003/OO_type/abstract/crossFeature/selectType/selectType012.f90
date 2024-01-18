! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Select Type Construct with array
!*                               if several CLASS IS type guard statements
!*                               match the selector, one of these statements
!*                               must specify a type that is an extension of
!*                               all the types specified in the others;
!*                               the block following that statement is executed.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

module m

   type, abstract :: b1
      integer :: i
   end type

   type, abstract, extends(b1) :: b2
   end type

   type, abstract, extends(b2) :: b3
   end type

   type, extends(b3) :: b4
   end type

end module

program selectType012
   use m

   class(b1), allocatable :: b11(:)

   allocate (b11(3), source = (/(b4(i),i=7,9)/))

   select type( b => b11 )
      class is (b1)
         error stop 1_4
      class is (b2)
         error stop 2_4
      class is (b3)
         error stop 3_4
      class is (b4)
         if ( (b(1)%i .ne. 7) .or. (b(2)%i .ne. 8) .or. (b(3)%i .ne. 9) ) error stop 4_4
      class default
         error stop 5_4
   end select

end program
