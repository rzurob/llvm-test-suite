! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc005a30.f
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
!*  DATE                       : 05/09/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocate (structure constructor for derived
!                               type with allocatable components as the
!                               expr-source)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (8)
        integer, kind         :: k1
        real(k1), allocatable :: r1
    end type

    type base1(k2)    ! (8)
        integer, kind            :: k2
        integer(k2), allocatable :: ids(:)
    end type
end module

program falloc005a30
use m
    class (base(8)), pointer :: b1, b2(:)
    class(base1(8)), allocatable :: bb1, bb2(:)

    real(8), allocatable :: r1
    integer(8), allocatable :: ids(:)

    logical(4) precision_r8

    allocate (b1, source=base(8)(r1))

    allocate (r1, source=1.5_8)

    allocate (b2(0:1), source=base(8)(r1))

    !! verify b1, b2
    if (allocated (b1%r1)) error stop 1_4

    if ((.not. allocated (b2(0)%r1)) .or. (.not. allocated(b2(1)%r1))) &
            error stop 2_4

    if ((.not. precision_r8 (b2(0)%r1, 1.5_8)) .or. &
        (.not. precision_r8 (b2(1)%r1, 1.5_8)))     error stop 3_4

    !! work on bb1 and bb2
    allocate (ids(3), source= (/1_8, 2_8, 3_8/))
    allocate (bb1, source=base1(8)(ids))

    deallocate (ids)

    allocate (bb2(3), source=base1(8)(ids))

    !! verify bb1 and bb2
    if (.not. allocated (bb1%ids)) error stop 4_4
    if (any(bb1%ids /= (/1_8, 2_8, 3_8/))) error stop 5_4


    if (allocated(bb2(1)%ids) .or. allocated(bb2(2)%ids) .or. &
        allocated(bb2(3)%ids)) error stop 6_4
end
