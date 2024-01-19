! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/28/2005
!*
!*  DESCRIPTION                : allocate (unlimited poly rank-one array with
!                               source-expr of an array section)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program falloc510
    class(*), pointer :: x(:)
    integer i1(5)

    i1 = (/1, 2, 3, 4, 5/)

    allocate (x(2), source=i1(3::2))

    select type (x)
        type is (integer)
            if (any (x /= (/3,5/))) error stop 1_4
        class default
            error stop 2_4
    end select
end
