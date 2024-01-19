! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_poly/misc/fmisc034.f
! opt variations: -qnok -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/04/2005
!*
!*  DESCRIPTION                : miscellaneous item (defect 298128 & 301983)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fmisc034
    type A(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), allocatable :: data(:)
    end type

    type B(k2,n2)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n2
        class(*), pointer :: data(:)
    end type

    type (A(4,20)) a1

    inquire (iolength=i1) a1
    inquire (iolength=i2) B(4,20)(null())
end

