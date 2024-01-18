!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 11/25/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               This is a test case derived from
!                               dtparamExtends005.f.  Test the IO operation with
!                               derived type with type parameters.
!*
!*
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

program dtparamIO001    !<-- derived from dtparamExtends005
use m
    integer(1) :: RED, BLUE, YELLOW
    parameter (RED=1, BLUE=3, YELLOW=2)


    type (general_color_point (8, 2, colorKind = kind(RED)))  color16Point
    type (general_color_point (4, 3, colorKind = 2))  tureColorPoint

    color16Point%coordinates = (/1.5d0, 3.8d0/)
    color16Point%color%colorValue = RED

    tureColorPoint%coordinates = (/1.3e0, 2.1e0, 13.2e0/)
    tureColorPoint%color%colorValue = BLUE

    !! print out the data
    write (*, '(2f12.3,i5)') color16Point
    write (*, '(3f12.3,i5)') tureColorPoint
end
