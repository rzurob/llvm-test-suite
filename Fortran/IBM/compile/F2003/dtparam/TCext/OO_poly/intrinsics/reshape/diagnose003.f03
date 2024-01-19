! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_poly/intrinsics/reshape/diagnose003.f
! opt variations: -ql -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/08/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Diagnostic test case: PAD shall be of the same type and type
!*  parameters as SOURCE. (Note: people are still debating this
!*  precondition. Maybe the correct precondition is: SOURCE shall
!*  be type compatible with PAD. Right now, only disgnose the case
!*  where SOURCE is not type compatible with PAD.)
!*    SOURCE is poly derived type and PAD is intrinsic type.
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  ===================================================================
!*  REVISION HISTORY
!*                    MM/DD/YY :
!*                        Init :
!*                    Comments :
!*  ===================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type
end module

program diagnose003
use m
    class(Base(4)), pointer :: b1(:)
    allocate(b1(15), SOURCE=(/(Base(4)(i),i=1,15)/))

    print *, reshape(b1, (/3,6/), (/-1/), (/1,2/))
end
