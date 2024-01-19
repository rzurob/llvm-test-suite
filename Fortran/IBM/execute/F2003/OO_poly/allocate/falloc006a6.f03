! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : allocate (unlimited poly pointer array of rank
!                               two in allocate statement; use intrinisic types
!                               as the source-expr)
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

program falloc006a6
    class (*), pointer :: x2(:,:)

    !! test integer type
    allocate (x2(2,2), source=reshape((/1_8, 2_8, 3_8, 4_8/), (/2,2/)))

    select type (x2)
        type is (integer(8))
            if (any(x2(1,:) /= (/1_8, 3_8/))) error stop 1_4
            if (any(x2(2,:) /= (/2_8, 4_8/))) error stop 2_4
        class default
            error stop 3_4
    end select

    deallocate (x2)

    !! test character type
    allocate (x2(2,2), source=reshape ((/'test 01', 'test 02', 'test 03', &
                'test 04'/), (/2,2/)))

    select type (y => x2(1,1:))
        type is (character(*))
            if (len(y) /= 7) error stop 4_4

            if ((y(1) /= 'test 01') .or. (y(2) /= 'test 03')) error stop 5_4
        class default
            error stop 10_4
    end select
end
