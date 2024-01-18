! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (finalization sequence; step 2 vs
!                               step 3)
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
    type dataType
        logical :: flag

        contains

        final :: finalizeData
    end type

    type base
        integer*4 :: id

        contains

        final :: finalizeBase
    end type

    type, extends(base) :: child
        type (dataType) :: data1
        class (dataType), allocatable :: data2
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeData (d)
        type (dataType), intent(in) :: d

        print *, 'finalizeDataType'
    end subroutine
end module

program ffinal525
    print *, 'call abc'

    call abc

    print *, 'call cba'

    call cba

    print *, 'end'
end

subroutine abc
use m
    type (child) :: c1
end subroutine

subroutine cba
use m
    type (child) :: c1

    allocate (c1%data2)
end subroutine
