! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self /tstdev/OO_poly/misc/fmisc010b1.f
! opt variations: -qck -qnok -ql -qdefaultpv -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 277245; problem #3:
!                               structure constructor with allocatable component
!                               used in an implied-do in an array constructor)
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

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(n1)    ! (4,15)
        integer, len  :: n1
        character(n1) :: name

        contains

        procedure :: print => printChild
    end type

    type dataType(k2)    ! (4)
        integer, kind                :: k2
        class(base(k2)), allocatable :: data

        contains

        procedure :: print => printDataType
    end type

    contains

    subroutine printBase (b)
        class(base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printDataType (d)
        class (dataType(4)), intent(in) :: d

        call d%data%print
    end subroutine
end module

program fmisc010b1
use m
    implicit none
    type (dataType(4)) :: d1 (10)
    integer i

    do i = 1, 10
!    d1 = (/(dataType(4)(child(4,15)(i, 'temp')), i = 1, 10)/)
        d1(i) = dataType(4)(child(4,15)(i, 'temp'))
    end do

    ! verify the results
    do i = 1, 10
        call d1(i)%print
    end do
end