! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self /tstdev/F2003/mv_Alloc/uLimitPoly/unlimitpolyA.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/31/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM and TO are unlimit polymorphic,
!*                               rank = 7
!*                               zero size for some rank
!*                               FROM and TO are components of different levels
!*                               of subobject of derived-types
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)      id
   end type

   type A(k2)    ! (4)
       integer, kind :: k2
       class(*), allocatable, dimension(:,:,:,:,:,:,:) ::  c2
   end type

   type(base(4)) base(4)

end module

program main
use m

   type :: B(k3)    ! (4)
       integer, kind            :: k3
       type(A(k3)), allocatable :: tp
       class(*), allocatable :: c1(:,:,:,:,:,:,:)
   end type

   type(B(4)) b1

   allocate( base(4) :: b1%c1(1,0,0,0,0,0,1) )
   if ( .not. allocated(b1%c1) ) error stop 21

   allocate( b1%tp )
   if ( .not. allocated(b1%tp) ) error stop 22

   allocate( complex :: b1%tp%c2(1,2,3,4,5,6,7))

   if ( .not. allocated(b1%tp%c2) ) error stop 23

   call move_alloc( b1%c1, b1%tp%c2 )

   if ( .not. allocated(b1%tp%c2) ) error stop 31

   if ( allocated(b1%c1) ) error stop 33

   print *, same_type_as(b1%c1, base(4)), shape(b1%c1)

end
