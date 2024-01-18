! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Select Type Construct with array
!*   Variable of poly abstract type, or non-poly extension of abstract type
!*      i.   CLASS is abstract type
!*      ii.  CLASS is abstract type and CLASS is extension type, and put zzrc in CLASS is abstract type
!*      iii. CLASS DEFAULT, access the derived type component
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

module m
   type, abstract :: base
      integer :: i = 5
   contains
      procedure, nopass :: print => printbase
   end type

   type, extends(base) :: child
   contains
      procedure, nopass :: print => printchild
   end type

contains

   integer function printbase()
      printbase = 1
   end function

   integer function printchild()
      printchild = 2
   end function

end module

program selectType010
   use m

   class(base), pointer, dimension(:) :: b1
   class(child), allocatable, target, dimension(:) :: c1

   allocate ( c1(3), source = (/ (child(i), i=1,3) /) )

   b1 => c1 (1:3:2)

   select type ( b => b1((/2/)) )
      class is (base)
         if (b(1)%print() .ne. 2) error stop 1_4
         if (b(1)%i .ne. 3) error stop 2_4
   end select

   select type ( b => b1(1:2:2) )
      class is (base)
         error stop 3_4
      class is (child)
         if (b(1)%print() .ne. 2) error stop 4_4
         if (b(1)%i .ne. 1) error stop 5_4
   end select

   select type ( b => b1 )
      class default
         if (b(1)%print() .ne. 2) error stop 6_4
         if (b(1)%i .ne. 1) error stop 7_4
         if (b(2)%print() .ne. 2) error stop 8_4
         if (b(2)%i .ne. 3) error stop 9_4
   end select

end program
