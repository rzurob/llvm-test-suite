! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  Pointer assignment
!*                               a) Scalar pointer and target
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

program ptrAsgn002
   use m

   class(base), pointer :: b1, b2
   class(base), allocatable, target :: b3

   allocate (b3, source = child(5))

   b2 => b3
   b1 => b3

   if (.not. associated(b1,b2) ) error stop 1_4

   if (b1%type() .ne. 2) error stop 2_4
   if (b2%type() .ne. 2) error stop 3_4

   nullify(b2)

   b1 => b2

   if (b1%type() .ne. 1) error stop 4_4
   if (b2%type() .ne. 1) error stop 5_4

end program
