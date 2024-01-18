!* =====================================================================
!* DATE                       : 12/08/2004
!* ORIGIN                     :
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
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type

    type Unknown
        character(10) :: c
    end type
end module

program diagnose004
use m
    class(Base), pointer :: b1(:)
    allocate(b1(15), SOURCE=(/(Base(i),i=1,15)/))

    associate(name1=>reshape(b1, (/3,6/), (/Unknown("a")/), (/1,2/)))
    end associate
end
