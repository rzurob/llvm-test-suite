! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/userDefOp/userDefOp005.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  User-defined operator and assignment
!*                               a) both operands are polymorphic abstract type for the operator in the interface and supplying
!*                                  5) array abstract declared type and extension declared type
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
   end type

   type, extends(base) :: child(k2,n1)    ! (4,4,20)
       integer, kind :: k2
       integer, len  :: n1
   end type

   interface operator(+)
      function myAdd1(a,b)
         import base, child
         class(base(4)), intent(in), dimension(:) :: a, b
         type(child(4,4,20)) :: myAdd1 (size(a))
      end function
   end interface

   interface assignment(=)
      subroutine myAsgn1(a,b)
         import base, child
         class(base(4)), intent(out), dimension(:) :: a
         class(base(4)), intent(in), dimension(:)  :: b
      end subroutine
   end interface

end module

program userDefOp005
   use m

   class(base(4)), dimension(:), allocatable :: c1, c3, c4
   type(child(4,4,20)), dimension(:), allocatable :: c2

   allocate(c1(2), source=(/ child(4,4,20)(3), child(4,4,20)(3) /) )
   allocate(c2(2), source=(/ child(4,4,20)(1), child(4,4,20)(1) /)  )
   allocate(c3(2), source=(c1+c2) )
   allocate(c4(2), source=(c1+c2+c3) )

   if ( ( c3(1)%id .ne. 4 ) .or. ( c3(2)%id .ne. 4 ) ) error stop 1_4
   if ( ( c4(1)%id .ne. 8 ) .or. ( c4(2)%id .ne. 8 ) ) error stop 2_4

end program

function myAdd1(a,b)
   use m, only: base, child
   class(base(4)), intent(in), dimension(:) :: a, b
   type(child(4,4,20)) :: myAdd1(size(a))

   if ( size(a) .eq. size(b) ) then
      myAdd1%id = a%id + b%id
   else
      error stop 3_4
   end if
end function

subroutine myAsgn1(a,b)
   use m, only: base, child
   class(base(4)), intent(out), dimension(:) :: a
   class(base(4)), intent(in), dimension(:)  :: b

   if ( size(a) .eq. size(b) ) then
      a%id = b%id
   else
      error stop 4_4
   end if
end subroutine
