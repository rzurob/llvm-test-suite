! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/22/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (finalizations of temps created by
!*                               function calls in if-construct)
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
        integer*4 :: id = -1
    end type

    type dataType
        class (base), pointer :: data => null()

        contains

        final :: finalizeData
    end type

    type (dataType), save :: d1_m

    contains

    subroutine finalizeData (d)
        type (dataType), intent(inout) :: d
        integer*4 err

        print *, 'finalizeData'

        if (associated (d%data)) then
            print *, 'deallocating d%data'
            deallocate (d%data, stat=err)
        end if
    end subroutine
end module

module m1
use m, only : base
    type, extends (base) :: child
        integer*4, pointer :: value => null()

        contains

        final :: finalizeChild
    end type

    contains

    subroutine finalizeChild (c)
        type (child), intent(inout) :: c
        integer*4 err

        print *, 'finalizeChild'

        if (associated (c%value)) then
            print *, 'deallocating c%value'
            deallocate (c%value, stat=err)
        end if
    end subroutine
end module

module m2
    interface makeData
        function makeDataOfBase (id)
        use m
            type (dataType) makeDataOFBase
            integer*4, intent(in) :: id
        end function

        function makeDataOfChild (id, value)
        use m
        use m1, only : child
            type (dataType) makeDataOfChild
            integer*4, intent(in) :: id, value
        end function
    end interface

    interface operator (==)
        logical function dataTypeEqual (d1, d2)
            use m
            type (dataType), intent(in) :: d1, d2
        end function
    end interface
end module

program ffinal514a18_1
use m
use m1, only: child
use m2
    type (child), target :: c1

    if (d1_m == makeData (1)) then
        print *, 'error occurs'
        error stop 1_4
    end if

    print *, ''


    if (d1_m == dataType(c1)) then
        print *, 'error occurs'
        error stop 2_4
    end if

    print *, ''

    if (makeData (1) == makeData (10)) then
        print *, 'error occurs'
        error stop 3_4
    end if

    print *, ''

    if (makeData (1) == makeData (2, 1)) then
        print *, 'error occurs'
        error stop 4_4
    end if

end

function makeDataOfBase (id)
use m
    type (dataType) makeDataOFBase
    integer*4, intent(in) :: id

    allocate (makeDataOfBase%data)

    makeDataOfBase%data%id = id
end function

function makeDataOfChild (id, value)
use m
use m1, only : child
    type (dataType) makeDataOfChild
    integer*4, intent(in) :: id, value

    type (child), pointer :: temp

    allocate (temp)
    allocate (temp%value)

    temp%id = id
    temp%value = value

    makeDataOfChild%data => temp
end function


logical function dataTypeEqual (d1, d2)
    use m
    type (dataType), intent(in) :: d1, d2

    if ((.not. associated(d1%data)) .and. (.not. associated(d2%data))) then
        dataTypeEqual = .true.
    else if (associated (d1%data) .and. associated(d2%data)) then
        dataTypeEqual = (d1%data%id == d2%data%id)
    else
        dataTypeEqual = .false.
    end if
end function
