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
!*  DATE                       : 01/03/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Kind type parameters participate the
!                               generic resolution: poly-arguement; one with
!                               kind type parameter and the other has none (base
!                               type is parameterless)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer id
    end type

    type, extends(base) :: child(k)
        integer, kind :: k

        real(k) data
    end type

    interface print
        module procedure printBase
        module procedure printChild4    !<-- unresolvable
        module procedure printChild8    !<-- unresolvable
    end interface

    contains

    subroutine printBase (b)
        class(base), intent(in) :: b
    end subroutine

    subroutine printChild4 (c)
        class(child(4)), intent(in) :: c
    end subroutine

    subroutine printChild8 (c)
        class(child(8)), intent(in) :: c
    end subroutine
end module

program kindparamGenerics001d
end
