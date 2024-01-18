! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp /tstdev/OO_poly/misc/fmisc008a1.f
! opt variations: -qnol -qdeferredlp

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fmisc008a1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 288894)
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id = -1
    end type

end module

program fmisc008a1
use m
    type (base(20,4)), allocatable :: b1(:)

    allocate (b1(2:5))

    b1%id = (/2,3,4,5/)

    associate (x => b1(3:)%id)
        if (size (x) /= 3) error stop 1_4

        if (any (x /= (/3,4,5/))) error stop 2_4
    end associate

end
