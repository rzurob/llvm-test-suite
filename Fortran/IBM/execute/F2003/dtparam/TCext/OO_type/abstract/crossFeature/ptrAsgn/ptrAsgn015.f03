! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/ptrAsgn/ptrAsgn015.f
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

   type, abstract :: base(k1)    ! (4)
      integer, kind            :: k1
      integer(k1)              :: id
      class(base(k1)), pointer :: ptr => null()
   contains
      procedure, nopass :: type => basetype
   end type

   type, extends(base) :: child(k2,n1)    ! (4,4,20)
       integer, kind :: k2
       integer, len  :: n1
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

program ptrAsgn015
   use m

   class(base(4)), pointer :: b1(:), b2(:)
   class(child(4,4,20)), pointer :: c1(:)
   class(child(4,4,20)), allocatable, target :: c2(:)

   type(child(4,4,20)), target :: c3 = child(4,4,20)(6,null())

   allocate (c2(2),  source=(/(child(4,4,20)(i,c3),i=5,6)/) )

   c1 => c2
   b2 => c2
   b1 => c1

   if (.not. associated(b1(1)%ptr,b2(1)%ptr) ) error stop 1_4
   if (.not. associated(b1(2)%ptr,b2(2)%ptr) ) error stop 1_4

   if (b1(1)%ptr%type() .ne. 2) error stop 2_4
   if (b1(2)%ptr%type() .ne. 2) error stop 2_4
   if (b2(1)%ptr%type() .ne. 2) error stop 3_4
   if (b2(2)%ptr%type() .ne. 2) error stop 3_4

   nullify(c1(1)%ptr,c1(2)%ptr)

   b1 => b2

   if (b1(1)%ptr%type() .ne. 1) error stop 4_4
   if (b1(2)%ptr%type() .ne. 1) error stop 4_4
   if (b2(1)%ptr%type() .ne. 1) error stop 5_4
   if (b2(2)%ptr%type() .ne. 1) error stop 5_4

   b1 => c1

   if (b1(1)%ptr%type() .ne. 1) error stop 6_4
   if (b1(2)%ptr%type() .ne. 1) error stop 6_4

end program