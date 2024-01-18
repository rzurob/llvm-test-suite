! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp /tstdev/OO_poly/misc/fmisc010a.f
! opt variations: -qnol -qdeferredlp

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fmisc010a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 277245; problem #2:
!                               ICE for array constructor containing structure
!                               constructor, followed by the usage of the same
!                               type with initialization)
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

program fmisc010a
use m
    type (base(20,4)) :: b1(10)
    type (base(20,4)), allocatable :: b2(:)

    b1 = (/(base(20,4)(i), i= 1, 10)/)

    allocate (b2(10))

    if (any (b1%id /= (/(j, j=1,10)/))) error stop 1_4

    if (any (b2%id /= -1)) error stop 2_4
end
