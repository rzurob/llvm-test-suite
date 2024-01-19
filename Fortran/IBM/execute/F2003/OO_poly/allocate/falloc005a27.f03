! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (unlimited poly allocatable array as
!                               allocate-object with source-expr)
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

program falloc005a27
    class (*), allocatable :: x1(:)

    integer i1 (10)

    i1 = (/(i, i=1,10)/)

    allocate (x1(2), source=i1(1:2))

    !! verify the result
    select type (x1)
        type is (integer)
            if (any(x1 /= (/1,2/))) error stop 1_4
        class default
            error stop 2_4
    end select
end
