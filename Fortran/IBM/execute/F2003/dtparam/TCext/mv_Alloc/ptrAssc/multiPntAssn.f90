! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/mv_Alloc/ptrAssc/multiPntAssn.f
! opt variations: -qnok -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : After executing multi pointer assignments
!*                               test if TO is associated with the pointer in
!*                               the last pointer assignemnts
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type A(n1,k1)    ! (20,4)
       integer, kind :: k1
       integer, len  :: n1
       integer(k1)      id
   end type

   type, extends(A) :: B(k2,n2)    ! (20,4,4,20)
       integer, kind :: k2
       integer, len  :: n2
   end type

   type, extends(B) :: C(k3,n3)    ! (20,4,4,20,4,20)
       integer, kind :: k3
       integer, len  :: n3
    end type

end module

use m

    type(C(:,4,4,:,4,:)), target, allocatable :: c1
    class(A(:,4)), target, allocatable :: a1

    class(A(:,4)), pointer :: p1
    class(B(:,4,4,:)), pointer :: p2
    type(C(:,4,4,:,4,:)), pointer :: p3
    class(*), pointer :: p0

    allocate(c1, source = C(20,4,4,20,4,20)(99))
    allocate(a1, source = A(20,4)(8))

    p3 => c1
    p2 => p3
    p1 => p2
    p0 => p1

    call move_alloc(c1, a1)

    if ( allocated(c1) ) stop 11
    if ( .not. allocated(a1) ) stop 13

    if ( .not. associated (p0, a1 )) stop 23

    select type (p0)
        type is (C(*,4,4,*,4,*))
            if ( p0%id /= 99 ) stop 25
        class default
            stop 33
    end select

end
