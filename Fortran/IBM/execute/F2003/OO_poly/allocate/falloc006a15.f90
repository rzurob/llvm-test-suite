! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/17/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : allocate (allocate unlimited poly pointer array
!                               using a function call that returns a poly
!                               allocatable array)
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
    type base
    end type

    type, extends(base) :: child
        integer id
        character(18) name
    end type

    contains

    class (base) function produceBaseAllocArray (b1)
        class (base), intent(in) :: b1(:)

        allocatable produceBaseAllocArray(:)

        allocate (produceBaseAllocArray(size(b1)), source=b1)
    end function
end module

program falloc006a15
use m
    class (*), pointer :: x1(:)

    allocate (x1(3), source=produceBaseAllocArray((/child(1,'test1'), &
                child(2,'test2'), child(3, 'test3')/)))

    select type (y => x1(::2))
        type is (child)
            if (any(y%id /= (/1, 3/))) error stop 1_4

            if (any(y%name /= (/'test1', 'test3'/))) error stop 2_4
        class default
            error stop 3_4
    end select
end
