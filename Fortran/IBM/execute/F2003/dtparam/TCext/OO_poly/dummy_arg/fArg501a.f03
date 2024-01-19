! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg501a.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/25/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (defined assignment used
!                               in forall construct)
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
    type dataType(k1)    ! (4)
        integer, kind        :: k1
        integer(k1), pointer :: data => null()

        contains

        procedure :: assgnData
        procedure :: value => dataTypeValue
    end type

    private dataTypeValue, assgnData
    contains

    pure integer*4 function dataTypeValue (d)
        class (dataType(4)), intent(in) :: d

        if (associated (d%data)) then
            dataTypeValue = d%data
        else
            dataTypeValue = 0   !<-- we treat null() as if 0
        end if
    end function

    pure subroutine assgnData (d, d1)
        class (dataType(4)), intent(inout) :: d
        class (dataType(4)), intent(in) :: d1

        if (.not. associated (d%data)) allocate (d%data)

        d%data = d1%value()
    end subroutine
end module

program fArg501a
use m
    interface assignment(=)
        pure subroutine dataAssign (d, d1)
        use m
            type (dataType(4)), intent(inout) :: d
            type (dataType(4)), intent(in) :: d1
        end subroutine
    end interface

    type (dataType(4)) :: dt (100)

    do i=1,50
        allocate (dt(i)%data)

        dt(i)%data = i
    end do

    forall (i=51:100)
        dt(i) = dt(101-i)
    end forall

    do i = 1,50
        if (dt(i)%value() /= i) error stop 1_4
    end do

    do i= 51,100
        if (dt(i)%value() /= 101-i) error stop 2_4
    end do

    do i = 1, 100
        deallocate (dt(i)%data)
    end do
end

pure subroutine dataAssign (d, d1)
use m
    type (dataType(4)), intent(inout) :: d
    type (dataType(4)), intent(in) :: d1

    call d%assgnData(d1)
end subroutine
