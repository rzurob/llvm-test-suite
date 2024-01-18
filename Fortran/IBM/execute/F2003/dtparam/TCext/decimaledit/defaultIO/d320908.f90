! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv /tstdev/F2003/decimaledit/defaultIO/d320908.f
! opt variations: -qnol -qdefaultpv -qreuse=self

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/02/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 320908)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A(n1,k1)    ! (20,8)
        integer, kind :: k1
        integer, len  :: n1
        complex(k1)   :: cx(3)
    end type

    type B(n2,k2,k3,k4,k5)    ! (20,4,4,4,8)
        integer, kind  :: k2,k3,k4,k5
        integer, len   :: n2
        integer(k2)       i
        real(k3)          r1
        logical(k4)       j(2)
        type(A(n2,k5)) :: a1 = A(20,k5) ((/1.0, 2.0, 3.0/))
    end type
end module

program commaEdit006
use m
    type(B(20,4,4,4,8)) b1, b2

    b1 = B(20,4,4,4,8)(1, 1.0, mod(1,2)==1)

    print *, b2%a1
    print *, b1
    write (*,*) B(20,4,4,4,8)(1, 1.0, mod(1,2)==1)
end
