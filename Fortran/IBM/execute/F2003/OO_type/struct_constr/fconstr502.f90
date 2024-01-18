!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr502.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/25/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (elemental function used
!                               for the component data source)
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
    type base
        integer(4) :: id
    end type

    type container
        class (base), allocatable :: data (:)
    end type

    contains

    elemental type (base) function addTwo (b1, b2)
        class (base), intent(in) :: b1, b2

        addTwo%id = b1%id + b2%id
    end function
end module

program fconstr502
use m
    class (base), allocatable :: b1(:)

    type (container) :: co1

    allocate (b1(10))

    b1%id = (/(i,i=1,10)/)

    co1 = container (addTwo(base (100), b2=b1))

    if (.not. allocated (co1%data)) error stop 1_4

    if (size (co1%data) /= 10) error stop 2_4

    if (any (co1%data%id /= (/(i,i=101,110)/))) error stop 3_4
end
