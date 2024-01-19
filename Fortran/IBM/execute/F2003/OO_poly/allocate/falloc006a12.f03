! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/17/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : allocate (function reference as source-expr for
!                               unlimited poly allocatable array; function is
!                               elemental)
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
    contains

    elemental integer(2) function createIDs (id, isize)
        integer (4), intent(in) :: id, isize

        createIDs = id+isize-1
    end function
end module

program falloc006a12
use m
    integer(4) :: isize(10) = (/(i, i=1,10)/)
    class (*), allocatable :: x1(:)

    allocate (x1(size(isize)), source=createIDs(100, isize))

    select type (x1)
        type is (integer(2))
            if (any (x1 /= (/(i, i=100, 109)/))) error stop 1_4
        class default
            error stop 2_4
    end select
end
