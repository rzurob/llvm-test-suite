! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/ptrAsgn/ptrAsgn001.f
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
!*                               unlimited polymorphic      polymorphic abstract type
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
       integer, kind :: k1
       integer(k1)   :: id
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

program ptrAsgn001
   use m
   class(*), pointer                :: u1
   class(base(4)), pointer             :: b1
   class(base(4)), allocatable, target :: b2

   allocate( b2, source=child(4,4,20)(5) )

   b1 => b2
   u1 => b2

   if (.not. associated(u1,b1) ) error stop 1_4
   if (.not. associated(u1,b2) ) error stop 2_4

   if (b1%type() .ne. 2) error stop 3_4

   select type(u1)
      type is (child(4,4,*))
         if (u1%type() .ne. 2) error stop 4_4
      class default
         error stop 5_4
   end select

   nullify(b1)

   u1 => b1

   select type(u1)
      class is (base(4))
         error stop 6_4
      type is (child(4,4,*))
         error stop 7_4
      class default
   end select

end program