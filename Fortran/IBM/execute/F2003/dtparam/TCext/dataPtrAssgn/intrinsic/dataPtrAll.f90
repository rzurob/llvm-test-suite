! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/intrinsic/dataPtrAll.f
! opt variations: -qnol -qnodeferredlp

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
!* - data-pointer X, a derived-type, has alloctable component(X%P)
!* - data-pointer has intent(in) attr, it's allocatable component is
!*     redefined by X%P = X%P(ub:lb:-1)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type A(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        logical(k1)   :: x
    end type

    interface
        subroutine sub(p)
            import A
            type(A(*,4)), intent(in), pointer :: p(:)
        end subroutine
    end interface
end module

    program main
        use m
        type(A(20,4)), pointer :: p(:)
        class(A(:,4)), target, allocatable :: tar(:)

        allocate(tar(10), source = (/ (A(20,4)(mod(i,2) ==0), i=1,10) /))

        p(size(tar):) => tar(2::2)

        if ( .not. associated(p, tar(2::2))) error stop 11
        if ( lbound(p,1) /= 10 ) error stop 13
        if ( ubound(p,1) /= 14 ) error stop 15

        call sub(p)

	if ( any (all(p%x) .neqv. (/ .true./))) error stop 17


    end program

        subroutine sub(p)
            use m, only: A
            type(A(*,4)), intent(in), pointer :: p(:)

            p%x = p(ubound(p,1):lbound(p,1):-1)%x
        end subroutine
