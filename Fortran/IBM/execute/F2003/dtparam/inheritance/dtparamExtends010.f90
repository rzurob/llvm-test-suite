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
!*  DATE                       : 11/29/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: type parameters can be declared in the
!                               extended type with the same name as inaccessible
!                               components in the parent type definition.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer, private :: k = -1
        real, allocatable, private :: l
    end type

    interface base      !<-- viewed as constructor for base type
        module procedure genBase
    end interface

    contains

    type(base) function genBase (i, r)
        integer, intent(in) :: i
        real, intent(in) :: r

        genBase%k = i
        allocate(genBase%l, source=r)
    end function

    subroutine printBase (b)
        class(base), intent(in) :: b

        if (allocated(b%l)) then
            write (*, '(i5,f12.2)') b%k, b%l
        else
            print *, b%k, 'unallocated'
        end if
    end subroutine
end module

module m1
use m
    type, extends(base) :: child (k, l) !<-- legal to use k and l here again
        integer, kind :: k
        integer, len :: l

        real(k) data(0:l)

        integer(k) id
    end type

    type (base), save :: b1_m(2)
    type (child(8, 2)), save :: c1_m
end module

program dtparamExtends010
use m1
    integer(8), parameter :: int64Bound = 2_8**32_8

    logical (4) precision_r8


    b1_m(2) = genBase(10, 2.5e0)

    c1_m%base = genBase(20, 3.2e0)
    c1_m%data = (/3.3d0, 7.5d0, -1.4d0/)
    c1_m%id = int64Bound + 10

    !! print out the values

    call printBase (b1_m(1))
    call printBase (b1_m(2))

    print *, 'test 2'

    call printBase (c1_m%base)

    if (c1_m%id - 12 /= int64Bound - 2) error stop 1_4

    if (.not. precision_r8 (c1_m%data(0), 3.3d0)) error stop 2_4
    if (.not. precision_r8 (c1_m%data(1), 7.5d0)) error stop 3_4
    if (.not. precision_r8 (c1_m%data(2), -1.4d0)) error stop 4_4
end
