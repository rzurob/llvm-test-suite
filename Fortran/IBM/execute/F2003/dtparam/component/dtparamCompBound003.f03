! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/19/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3: components)
!                               Case: Bounds of derived type component can be
!                               specification expression: use intrinsic
!                               functions and length type parameters.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type asciiMap (pos)
        integer(1), len :: pos

        !! it's silly for this declaration, but it's perfectly legal
        character, dimension(0:iachar(achar(pos))) :: map
    end type

    contains

    subroutine initMap (map)
        type (asciiMap(*)), intent(out) :: map

        map%map = (/(achar(i), i=0, map%pos)/)
    end subroutine
end module

program dtparamCompBound003
use m
    type (asciiMap(:)), allocatable :: map

    allocate (asciiMap(88):: map)

    call initMap (map)

    if (any (map%map(83:88) /= (/'S', 'T', 'U', 'V', 'W', 'X'/))) error stop 1_4

    deallocate (map)

    allocate (asciiMap(122) :: map)

    call initMap (map)

    if (map%map(map%pos) /= 'z') error stop 2_4
end
