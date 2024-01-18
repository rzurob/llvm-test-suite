! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc005a5.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (array element as the source-expr)
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

program falloc005a5
    type base(k1)    ! (8)
        integer, kind             :: k1
        integer(k1) , allocatable :: data(:)
    end type

    type (base(8)) :: b1
    integer(8) :: i1 (6)

    i1 = (/1,2,3,4,5,6/)

    allocate (b1%data(0:7), source= i1(3))

    if (any (b1%data /= 3_8)) error stop 1_4
end
