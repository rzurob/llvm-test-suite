! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-07 (original: 02/14/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
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
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1) :: id = -1
    end type

    type dataType (kdataType_1) ! kdataType_1=4
       integer, kind :: kdataType_1
        class (base(kdataType_1)), pointer :: data => null() ! tcx: (kdataType_1)

        contains

        final :: finalizeData
    end type

    type (dataType(4)), save :: d1_m ! tcx: (4)

    contains

    subroutine finalizeData (d)
        type (dataType(4)), intent(inout) :: d ! tcx: (4)

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
        integer(kbase_1), pointer :: value => null()

        contains

        final :: finalizeChild
    end type

    contains

    subroutine finalizeChild (c)
        type (child(4)), intent(inout) :: c ! tcx: (4)

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
            type (dataType(4)) makeDataOFBase ! tcx: (4)
            integer*4, intent(in) :: id
        end function

        function makeDataOfChild (id, value)
        use m
        use m1, only : child
            type (dataType(4)) makeDataOfChild ! tcx: (4)
            integer*4, intent(in) :: id, value
        end function
    end interface

    interface operator (==)
        logical function b1EqualB2 (b1, b2)
        use m1
            class (base(4)), intent(in) :: b1, b2 ! tcx: (4)
        end function

        logical function d1EqualD2 (d1, d2)
        use m
            class (dataType(4)), intent(in) :: d1, d2 ! tcx: (4)
        end function
    end interface
end module

program ffinal514a18k
use m1
use m2
    if (makeData (1, 10) == makeData (10, 1)) error stop 101_4

    if (makeData (1) == makeData (1)) print *, 'test 2'

    print *, 'end'
end

function makeDataOfBase (id)
use m
    type (dataType(4)) makeDataOFBase ! tcx: (4)
    integer*4, intent(in) :: id

    allocate (makeDataOfBase%data)

    makeDataOfBase%data%id = id
end function

function makeDataOfChild (id, value)
use m
use m1, only : child
    type (dataType(4)) makeDataOfChild ! tcx: (4)
    integer*4, intent(in) :: id, value

    type (child(4)), pointer :: temp ! tcx: (4)

    allocate (temp)
    allocate (temp%value)

    temp%id = id
    temp%value = value

    makeDataOfChild%data => temp
end function


logical function b1EqualB2 (b1, b2)
use m1
    class (base(4)), intent(in) :: b1, b2 ! tcx: (4)

    if (.not. same_type_as (b1, b2)) then
        b1EqualB2 = .false.
    else
        b1EqualB2 = b1%id == b2%id

        select type (b1)
            type is (base(4)) ! tcx: (4)
            type is (child(4)) ! tcx: (4)
                select type (b2)
                    type is (child(4)) ! tcx: (4)
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
    class (dataType(4)), intent(in) :: d1, d2 ! tcx: (4)

    if (associated (d1%data)) then
        d1EqualD2 = associated (d2%data)

        if (d1EqualD2) then
            d1EqualD2 = d1%data == d2%data
        end if
    else
        d1EqualD2 = .not. associated (d2%data)
    end if
end function


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
! type: dataType - added parameters (kdataType_1) to invoke with (4) / declare with (4) - 8 changes
! type: child - added parameters () to invoke with (4) / declare with (4) - 4 changes
