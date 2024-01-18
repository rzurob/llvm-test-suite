! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/F2003/mv_Alloc/typCompatible/poly4ToFmfinal.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : 1. FROM and TO are of poly type
!*                               2. rank-1
!*                               3. TO is finalized
!*		                 4. FROM is zero-size array
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    integer  :: numA = 0, numB = 0

    type A(n1,k1)    ! (20,4)
	integer, kind :: k1
	integer, len  :: n1
	integer(k1)   :: iA
	contains
	    final :: finalA
    end type

    type, extends(A) :: B    ! (20,4)
	integer(k1) :: iB
        type(A(n1,k1)) :: x
        contains
            final :: finalB
    end type

    contains
        subroutine finalA(a)
            type(A(*,4)), intent(in) :: A
            numA = numA + 1
        end subroutine

        subroutine finalB(a)
	    type(B(*,4)), intent(in) :: A
            numB = numB + 1
        end subroutine
end module


program main
use m

    class(A(:,4)), allocatable :: to(:)
    class(B(:,4)), allocatable :: from(:)

    allocate(B(20,4) :: to(1))
    allocate(B(20,4) :: from (0:-1) )

    select type (to)
        type is (B(*,4))
	    to(1)%iB = 1003
            to(1)%iA = 1002
            to(1)%x%iA = 1001

            to(1) = B(20,4)(iA = 101, iB = 102, x= A(20,4)(11))
    end select

    if ( numA /= 5 ) stop 11
    if ( numB /= 2 ) stop 13

    ! to is rank-1 array
    call move_alloc(from ,to )

    if ( .not. allocated(to) ) stop 21
    if ( allocated(from) ) stop 23

    select type (to)
	type is (B(*,4))
            if ( size(to) /= 0 ) stop 31
    end select

    if ( numA /= 7 ) stop 41
    if ( numB /= 2 ) stop 43

   end

