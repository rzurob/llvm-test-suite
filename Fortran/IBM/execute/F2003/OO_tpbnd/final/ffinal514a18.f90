!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/14/2005
!*
!*  DESCRIPTION                : final sub (finalization of the temps created by
!                               function return results (defined operator) in if
!                               statement)
!*
!*  KEYWORD(S)                 :
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

        print *, 'finalizeData'

        if (associated (d%data)) then
            print *, 'deallocating d%data'
            deallocate (d%data)
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

        print *, 'finalizeChild'

        if (associated (c%value)) then
            print *, 'deallocating c%value'
            deallocate (c%value)
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
        logical function b1EqualB2 (b1, b2)
        use m1
            class (base), intent(in) :: b1, b2
        end function

        logical function d1EqualD2 (d1, d2)
        use m
            class (dataType), intent(in) :: d1, d2
        end function
    end interface
end module

program ffinal514a18
use m1
use m2
    if (makeData (1, 10) == makeData (10, 1)) error stop 1_4

    if (makeData (1) == makeData (1)) print *, 'test 2'

    print *, 'end'
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


logical function b1EqualB2 (b1, b2)
use m1
    class (base), intent(in) :: b1, b2

    if (.not. same_type_as (b1, b2)) then
        b1EqualB2 = .false.
    else
        b1EqualB2 = b1%id == b2%id

        select type (b1)
            type is (base)
            type is (child)
                select type (b2)
                    type is (child)
                        if (associated (b1%value)) then
                            b1EqualB2 = b1EqualB2 .and. associated(b1%value, &
                                                        b2%value)

                        else
                            b1EqualB2 = b1EqualB2 .and. (.not. &
                                    associated(b2%value))
                        end if
                    class default
                        error stop 11_4
                end select
            class default
                error stop 10_4
        end select
    end if
end function


logical function d1EqualD2 (d1, d2)
use m
use m2, only : operator(==)
    class (dataType), intent(in) :: d1, d2

    if (associated (d1%data)) then
        d1EqualD2 = associated (d2%data)

        if (d1EqualD2) then
            d1EqualD2 = d1%data == d2%data
        end if
    else
        d1EqualD2 = .not. associated (d2%data)
    end if
end function
