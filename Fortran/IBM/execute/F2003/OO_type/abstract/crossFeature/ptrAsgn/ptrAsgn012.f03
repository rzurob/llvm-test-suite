! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  Pointer assignment
!*                               a) type component to type component
!*                               Left                       Right
!*                               polymorphic abstract type  non-polymorphic extension type
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

program ptrAsgn12
   use m

   class(base), pointer :: b1, b2
   type(child), pointer :: c1
   type(child), allocatable, target :: c2

   type(child), target :: c3 = child(6,null())

   allocate (c2, source = child(5,c3))

   c1 => c2
   b2 => c2
   b1 => c1

   if (.not. associated(b1%ptr,b2%ptr) ) error stop 1_4

   if (b1%ptr%type() .ne. 2) error stop 2_4
   if (b2%ptr%type() .ne. 2) error stop 3_4

   nullify(c1%ptr,b2%ptr)

   b1 => b2

   if (b1%ptr%type() .ne. 1) error stop 4_4
   if (b2%ptr%type() .ne. 1) error stop 5_4

   b1 => c1

   if (b1%ptr%type() .ne. 1) error stop 6_4

end program
