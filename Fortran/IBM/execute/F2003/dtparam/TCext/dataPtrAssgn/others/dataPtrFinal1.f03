! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/others/dataPtrFinal1.f
! opt variations: -qnok -qnol -qnodeferredlp

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-pointer/target are rank-1 array
!* - data-pointer is self referenced w bounds-spec/remapping-lst
!* - check if target is finalized
!*
!234567890123456789012345678901234567890123456789012345678901234567890


 module m

    integer :: countA = 0

    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
	contains
	    final :: finalA
    end type

    contains
	subroutine finalA(a)
	    type(base(4,*)), intent(inout) :: a(:)

	    countA = countA + 1
	    print *, "final subroutine :: ", lbound(a,1), ubound(a,1)
        end subroutine

 end module

 program main
    use m

    type(base(4,:)), pointer :: p(:)

    allocate(base(4,20) :: P(1024))
    p(3:) => p

    if ( .not. associated(p)) error stop 12
    deallocate(p)

    if ( associated(p) ) error stop 41
    if ( countA /= 1 ) error stop 43

    countA = 0

    allocate(base(4,20) :: P(512))
    p(7:518) => p(512:1:-1)
    if ( .not. associated(p)) error stop  45
    deallocate(p)

    if ( associated(p) ) error stop 41
    if ( countA /= 1 ) error stop 43

 End program