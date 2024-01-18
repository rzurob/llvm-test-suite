! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/misc/fmisc019a.f
! opt variations: -qnol

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/30/2005
!*
!*  DESCRIPTION                : miscellaneous items (defect 294132)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fmisc019a
    type base(n1,k1)    ! (20,8)
        integer, kind            :: k1
        integer, len             :: n1
        integer(k1), allocatable :: data(:)
    end type

    integer(8), allocatable :: i1(:)
    integer(8) k

    allocate (i1(0:7))

    i1 = (/(k,k=0_8,7_8)/)

    call associate_replacer (base(20,8)(i1(::2)), y = base(20,8)(i1))

    contains

!    associate (x => base(20,8)(i1(::2)), y => base(20,8)(i1))
    subroutine associate_replacer (x, y)
        type(base(*,8)), intent(in) :: x, y

        if (any (x%data /= (/0_8,2_8,4_8,6_8/))) error stop 1_4
        if (any (y%data /= i1)) error stop 2_4
    end subroutine
end
