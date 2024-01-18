! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/selectType/selectType005.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Select Type Construct
!*      Unlimited polymorphic allocated to be poly abstract type or nonpoly extension of abstract type
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
   type, abstract :: base(k1)    ! (4)
       integer, kind :: k1
       integer(k1)   :: i = 5
   contains
      procedure, nopass :: print => printbase
   end type

   type, extends(base) :: child(k2,n1)    ! (4,4,20)
       integer, kind :: k2
       integer, len  :: n1
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

program selectType005
   use m

   class(*), allocatable :: u1
   class(*), pointer :: u2

   class(base(4)), pointer :: b1
   type(child(4,4,20)), allocatable, target :: c1

   allocate ( c1, source = child(4,4,20)() )
   b1 => c1

   allocate (u1, source = b1 )
   allocate (u2, source = c1 )

   select type ( b => u1 )
      class is (base(4))
         if (b%print() .ne. 2) error stop 1_4
         if (b%i .ne. 5) error stop 2_4
   end select

   select type ( b => u1 )
      class is (base(4))
         error stop 3_4
      class is (child(4,4,*))
         if (b%print() .ne. 2) error stop 4_4
         if (b%i .ne. 5) error stop 5_4
   end select

   select type ( u2 )
      class is (base(4))
         if (u2%print() .ne. 2) error stop 6_4
         if (u2%i .ne. 5)       error stop 7_4
   end select

   select type ( u2 )
      class is (base(4))
         error stop 8_4
      class is (child(4,4,*))
         if (u2%print() .ne. 2) error stop 9_4
         if (u2%i .ne. 5) error stop 10_4
   end select

end program
