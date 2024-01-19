! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/23/2005
!*
!*  DESCRIPTION                : dtparameter (Section 4.5.6.1: inheritance)
!                               Statement: Additional type parameters may be
!                               declared in the definition of the extended type.
!                               Case: general_point, general_color and
!                               general_color_point
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type general_color (k)
        integer, kind :: k

        integer(k) :: colorValue
    end type

    type general_point (k, dim)
        integer, kind :: k
        integer, len :: dim

        real(k) :: coordinates(dim)
    end type

    type, extends(general_point) :: general_color_point (colorKind)
        integer, kind :: colorKind
        type(general_color(colorKind)) color
    end type
end module

program dtparamExtends005
use m
    integer(1) :: RED, BLUE, YELLOW
    parameter (RED=1, BLUE=3, YELLOW=2)

    logical(4) precision_r4, precision_r8

    type (general_color_point (8, 2, colorKind = kind(RED)))  color16Point
    type (general_color_point (4, colorKind = 2, dim=3))  tureColorPoint

    color16Point%coordinates = (/1.5d0, 3.8d0/)
    color16Point%color%colorValue = RED

    tureColorPoint%coordinates = (/1.3e0, 2.1e0, 13.2e0/)
    tureColorPoint%color%colorValue = BLUE

    !! verify the data components
    if (.not. precision_r8(color16Point%coordinates(1), 1.5d0)) error stop 1_4
    if (.not. precision_r8(color16Point%coordinates(2), 3.8d0)) error stop 2_4

    if (color16Point%color%colorValue /= RED) error stop 3_4


    if (.not. precision_r4(tureColorPoint%coordinates(1), 1.3e0)) error stop 4_4
    if (.not. precision_r4(tureColorPoint%coordinates(2), 2.1e0)) error stop 5_4
    if (.not. precision_r4(tureColorPoint%coordinates(3), 13.2e0)) error stop 6_4

    if (tureColorPoint%color%colorValue /= BLUE) error stop 7_4

end
