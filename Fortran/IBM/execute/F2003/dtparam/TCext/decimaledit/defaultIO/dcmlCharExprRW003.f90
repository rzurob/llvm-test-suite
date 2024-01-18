! GB DTP extension using:
! ftcx_dtp -qck -qnol -qreuse=base /tstdev/F2003/decimaledit/defaultIO/dcmlCharExprRW003.f
! opt variations: -qnock -ql -qreuse=self -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/07/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Use of the namelist for the format control with
!                               DECIMAL= specifier. test is for C928: format or
!                               namelist MUST also present if DECIMAL= appears
!                               in data transfer statement.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,k2)    ! (4,4)
        integer, kind :: k1,k2
        integer(k1)   :: id
        real(k2)      :: d(2)
    end type

    type, extends(base) :: child(k3,n1)    ! (4,4,1,20)
        integer, kind             :: k3
        integer, len              :: n1
        character(kind=k3,len=n1) :: name
        complex(k1)               :: cx
    end type
end module


program dcmlCharExprRW003
use m
    type(base(4,4)), allocatable :: b1(:)

    type(child(4,4,1,20)) c1

    namelist /nml1/ b1, c1

    allocate (b1(10), source=(/(base(4,4)(i, (/i, i*2/)), i=1,10)/))

    c1 = child(4,4,1,20) (100, 1.5, 'xlftest F2003', cmplx(-1, -10))

    write (*, nml1, decimal='COMMA')

    write (nml=nml1, unit=*, delim='APOSTROPHE', decimal='POINT')
end
