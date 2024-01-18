! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodeferredlp /tstdev/OO_poly/point_assgn/fpAssgn012a1.f
! opt variations: -qnok -qnol -qdeferredlp

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn012a1.f
! %VERIFY: fpAssgn012a1.out:fpAssgn012a1.vf
! %STDIN:
! %STDOUT: fpAssgn012a1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (in intrinsic
!                               assignment occurs in WHERE construct)
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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class (*), pointer :: data => null()
    end type

    interface operator (==)
        elemental logical function b1EQb2 (b1, b2)
            import base
            class (base(4,*)), intent(in) :: b1, b2
        end function
    end interface
end module

program fpAssgn012a1
use m
    complex(8), target :: c1 (3)
    real(4), target :: r1 (2)

    integer(4), target :: i1

    type (base(4,20)), allocatable :: b1(:)

    allocate (b1(10))

    forall (i=1:3) b1(i)%data => c1(i)

    forall (i=7:8) b1(i)%data => r1(i-6)

    where (b1 == base(4,20)(null()))  b1 = base(4,20)(i1)

    do i = 1, 10
        if (b1(i) == base(4,20)(i1))  print *, i
    end do

    do i = 1, 3
        if (.not. (b1(i) == base(4,20) (c1(i)))) error stop 1_4
    end do

    if (.not. (b1(7) == base(4,20)(r1(1))))  error stop 2_4
    if (.not. (b1(8) == base(4,20)(r1(2))))  error stop 3_4
end


!! for this comparison, we test if b1%data and b2%data are associated or if
!they're both disassoicated
elemental logical function b1EQb2 (b1, b2)
use m, only : base
    class (base(4,*)), intent(in) :: b1, b2

    if (.not. associated (b1%data)) then
        b1EQb2 = .not. associated (b2%data)
    else
        b1EQb2 = associated (b1%data, b2%data)
    end if
end function
