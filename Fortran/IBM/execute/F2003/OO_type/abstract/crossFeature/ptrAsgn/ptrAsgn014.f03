! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  Pointer assignment
!*                               a) type component to type component with array
!*                               Left                       Right
!*                               polymorphic abstract type  polymorphic abstract type
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
      class(base), pointer :: ptr => null()
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

program ptrAsgn014
   use m

   class(base), pointer :: b1(:), b2(:)
   class(base), allocatable, target :: b3(:)

   type(child), target :: c1 = child(6,null())

   allocate( b3(2), source=(/(child(i,c1),i=5,6)/) )

   b2 => b3
   b1 => b3

   if (.not. associated(b1,b2) ) error stop 1_4
   if (.not. associated(b1(1)%ptr, b2(1)%ptr) ) error stop 2_4
   if (.not. associated(b1(2)%ptr, b2(2)%ptr) ) error stop 2_4

   if ((b1(1)%ptr%type() .ne. 2) .or. (b2(1)%ptr%type() .ne. 2) ) error stop 3_4
   if ((b1(2)%ptr%type() .ne. 2) .or. (b2(2)%ptr%type() .ne. 2) ) error stop 3_4

   nullify(b2(1)%ptr)
   nullify(b2(2)%ptr)

   b1 => b2

   if (b1(1)%ptr%type() .ne. 1) error stop 4_4
   if (b1(2)%ptr%type() .ne. 1) error stop 4_4

   if (b1%type() .ne. 2) error stop 5_4
   if (b2%type() .ne. 2) error stop 6_4

end program