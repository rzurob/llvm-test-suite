!*  ===================================================================
!*
!*  DATE                       : 07/25/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE with DTP
!*                               (type-spec in allocate statement)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ==================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtParamAlloc02

    type base (k, n)
        integer, kind :: k
        integer, len  :: n

        integer :: data(n) = 0
    end type

    class (*), pointer :: x1(:)

    class (*), allocatable :: x2

    allocate (base(4, 2):: x1(2:4), x2)

    if ((.not. associated (x1)) .or. (.not. allocated (x2))) stop 1

    if ((lbound (x1, 1) /= 2) .or. (ubound (x1, 1) /= 4)) stop 2

    select type (x1)
      type is (base(4, *))
        select type (x2)
          type is (base(4, *))
            if ( (x1%n /=2) .or. (x2%n /=2) ) stop 4
          class default
            stop 5
        end select
      class default
        stop 6
    end select
end
