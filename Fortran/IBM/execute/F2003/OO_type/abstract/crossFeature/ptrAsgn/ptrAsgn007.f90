! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  Pointer assignment
!*                               a) Array pointer and target
!*                               Left                       Right
!*                               polymorphic abstract type  polymorphic extension type
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
      integer :: id
   contains
      procedure, nopass :: type => basetype
   end type

   type, extends(base) :: child
   contains
      procedure, nopass :: type => childtype
   end type

contains

   integer function basetype()
      basetype = 1
   end function

   integer function childtype()
      childtype = 2
   end function

end module

program ptrAsgn007
   use m

   class(base), pointer :: b1(:), b2(:)
   class(child), pointer :: c1(:)
   class(child), allocatable, target :: c2(:)

   allocate( c2(5), source=(/ (child(i),i=5,9) /) )

   c1 => c2
   b2 => c2
   b1 => c1

   if (.not. associated(b1,b2) ) error stop 1_4

   if (b1%type() .ne. 2) error stop 2_4
   if (b2%type() .ne. 2) error stop 3_4

   if ((size(b1) .ne. 5) .or. (size(b2) .ne. 5) ) error stop 4_4

   nullify(c1,b2)

   b1 => b2

   if (b1%type() .ne. 1) error stop 5_4
   if (b2%type() .ne. 1) error stop 6_4

   b1 => c1

   if (b1%type() .ne. 1) error stop 7_4

end program
