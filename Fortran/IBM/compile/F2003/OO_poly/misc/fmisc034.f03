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
    type A
        class(*), allocatable :: data(:)
    end type

    type B
        class(*), pointer :: data(:)
    end type

    type (A) a1

    inquire (iolength=i1) a1
    inquire (iolength=i2) B(null())
end
