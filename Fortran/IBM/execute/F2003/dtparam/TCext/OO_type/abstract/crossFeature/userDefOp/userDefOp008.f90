! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/userDefOp/userDefOp008.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  User-defined operator and assignment
!*                               User-defined assignment
!*                                  a) both left and right hand side of the assignment are poly abstract type with arrays
!*                                     Left         Right
!*                                     Abstract     Abstract
!*                                     Abstract     non-Abstract
!*                                     non-Abstract Abstract
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

program userDefOp008
   use m

   class(base(4)), allocatable, dimension(:) :: b1, b2, b3
   type(child(4,4,20)), allocatable, dimension(:) :: c1
   class(child(4,4,20)), allocatable, dimension(:) :: c2

   allocate( b1(2), source=(/child(4,4,20)(1), child(4,4,20)(1) /) )
   allocate( b2(2), source=(/child(4,4,20)(2), child(4,4,20)(2) /) )
   allocate( b3(2), source=(/child(4,4,20)(3), child(4,4,20)(3) /) )
   allocate( c1(2), source=(/child(4,4,20)(4), child(4,4,20)(4) /) )
   allocate( c2(2), source=(/child(4,4,20)(5), child(4,4,20)(5) /) )

   b1 = b2
   b2 = c1
   b3 = c2
   c1 = b3
   c2 = b2

   if ( (b1(1)%id .ne. 2) .or. (b1(2)%id .ne. 2) ) error stop 1_4
   if ( (b2(1)%id .ne. 4) .or. (b2(2)%id .ne. 4) ) error stop 2_4
   if ( (b3(1)%id .ne. 5) .or. (b3(2)%id .ne. 5) ) error stop 3_4
   if ( (c1(1)%id .ne. 5) .or. (c1(2)%id .ne. 5) ) error stop 4_4
   if ( (c2(1)%id .ne. 4) .or. (c2(2)%id .ne. 4) ) error stop 5_4

end program

function myAdd1(a,b)
   use m, only: base, child
   class(base(4)), intent(in), dimension(:) :: a, b
   type(child(4,4,20)) :: myAdd1(size(a))

   if ( size(a) .eq. size(b) ) then
      myAdd1%id = a%id + b%id
   else
      error stop 6_4
   end if
end function

subroutine myAsgn1(a,b)
   use m, only: base, child
   class(base(4)), intent(out), dimension(:) :: a
   class(base(4)), intent(in), dimension(:)  :: b

   if ( size(a) .eq. size(b) ) then
      a%id = b%id
   else
      error stop 7_4
   end if
end subroutine
