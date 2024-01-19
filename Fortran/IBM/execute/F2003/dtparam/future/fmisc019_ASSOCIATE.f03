! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/misc/fmisc019.f
! opt variations: -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous item (defect 294132)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fmisc019
    type base(n1,k1)    ! (20,8)
        integer, kind            :: k1
        integer, len             :: n1
        integer(k1), allocatable :: data(:)
    end type

    integer(8), allocatable :: i1(:)
    integer(8) k

    allocate (i1(0:7))

    i1 = (/(k,k=0_8,7_8)/)

    associate (x => base(20,8)(i1(::2)), y => base(20,8)(i1))
        print *, lbound(x%data), ubound(x%data)
        print *, lbound(y%data), ubound(y%data)
    end associate
end
