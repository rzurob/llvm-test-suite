! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/reshape/diagnose004.f
! opt variations: -qnock -qnol -qnodeferredlp -qreuse=none

!* =====================================================================
!* DATE                       : 12/08/2004
!* PRIMARY FUNCTIONS TESTED   : reshape
!* DESCRIPTION                :
!*   Diagnostic test case: PAD shall be of the same type and type
!* parameters as SOURCE. (Note: people are still debating this
!* precondition. Maybe the correct precondition is: SOURCE shall
!* be type compatible with PAD. Right now, only disgnose the case
!* where SOURCE is not type compatible with PAD.)
!*   SOURCE is poly derived type and PAD is another derived type.
!* =====================================================================
!* REVISION HISTORY
!*                   MM/DD/YY : 03/23/05
!*                       Init : yongdu@ca.ibm.com
!*                   Comments : 1) Removed TRUN header.
!*                              2) The old version tried to print out
!*                                 the return value of reshape, which
!*                                 is poly and should not be processed
!*                                 by regular IO. Use associate instead.
!*                              3) The verification file needs to be
!*                                 updated later.
!* =====================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j
    end type

    type Unknown(k2,n2)    ! (1,10)
        integer, kind             :: k2
        integer, len              :: n2
        character(kind=k2,len=n2) :: c
    end type
end module

program diagnose004
use m
    class(Base(:,4)), pointer :: b1(:)
    allocate(b1(15), SOURCE=(/(Base(20,4)(i),i=1,15)/))

    associate(name1=>reshape(b1, (/3,6/), (/Unknown(1,10)("a")/), (/1,2/)))
    end associate
end
