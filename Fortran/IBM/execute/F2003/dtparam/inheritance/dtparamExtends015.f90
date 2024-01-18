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
!                               Case: test the default values of the type
!                               parameters and the default initializations of
!                               the components.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    real(8), parameter :: defaultReal = 1.35d0
    complex(4), parameter :: defaultCmplx = (1.3e0, 2.2e0)
    character(*), parameter :: defaultName = 'default'

    type base (ki, kr, l)
        integer, kind :: ki = 4
        integer, kind :: kr = 8
        integer, len :: l = 10

        integer(ki), dimension(l) :: ids = -1
        real(kr) :: data = defaultReal
        integer(ki), pointer :: ptr(:) => null()
        real(kr), allocatable :: alloc(:,:,:)
    end type

    type, extends(base) :: child(n, kx)
        integer, kind :: kx = 4
        integer, len :: n = 20

        character(n) :: name = defaultName
        complex(kx) :: cx = defaultCmplx
    end type

    type (child), save :: c1_m
end module

program dtparamExtends015
use m
    type (child) :: c1(2)
    logical(4) precision_r8, precision_x8

    !! verify the default values: c1 and c1_m are of the same values
    if (associated(c1_m%ptr) .or. associated(c1(1)%ptr) .or. &
        associated(c1(2)%ptr)) error stop 1_4

    if (allocated (c1(1)%alloc) .or. allocated(c1(2)%alloc) .or. &
        allocated(c1_m%alloc)) error stop 2_4

    do i = 1, 10
        if ((c1_m%ids(i) /= -1) .or. (c1(1)%ids(i) /= -1) .or. &
            (c1(2)%ids(i) /= -1)) error stop 3_4
    end do


    if ((.not. precision_r8 (c1_m%data, defaultReal)) .or. &
        (.not. precision_r8 (c1(1)%data, defaultReal)) .or. &
        (.not. precision_r8 (c1(2)%data, defaultReal))) error stop 4_4


    if ((c1_m%name /= defaultName) .or. (c1(1)%name /= defaultName) .or. &
        (c1(2)%name /= defaultName)) error stop 5_4


    if ((.not. precision_x8(c1(1)%cx, defaultCmplx)) .or. &
        (.not. precision_x8(c1(2)%cx, defaultCmplx)) .or. &
        (.not. precision_x8(c1_m%cx, defaultCmplx)))  error stop 6_4

    if ((kind(c1%ids(1)) /= 4) .or. (kind(c1_m%data) /= 8) .or. &
        (kind(c1(1)%alloc) /= 8) .or. (kind(c1_m%ptr) /= 4) .or. &
        (kind(c1%cx) /= 4))     error stop 7_4


    if ((len(c1%name) /= 20) .or. (size(c1_m%ids) /= 10)) error stop 8_4
end
