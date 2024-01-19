! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/30/2005
!*
!*  DESCRIPTION                : miscellaneous (defect 294132)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fmisc019a1
    type base
        integer, allocatable :: data(:)
    end type

    integer(8), allocatable :: i1(:)
    integer(8) k

    allocate (i1(0:7))

    i1 = (/(k,k=0_8,7_8)/)

    associate (x => base(i1(::2)), y => base(i1))
        if (any (x%data /= (/0,2,4,6/))) error stop 1_4
        if (any (y%data /= i1)) error stop 2_4
    end associate
end
