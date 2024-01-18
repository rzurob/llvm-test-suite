! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal514a18_1k
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal514a18_1 by Jim Xia)
!*  DATE                       : 2007-11-07 (original: 04/22/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
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
        integer(kbase_1), pointer :: value => null()

        contains

        final :: finalizeChild
    end type

    contains

    subroutine finalizeChild (c)
        type (child(4)), intent(inout) :: c ! tcx: (4)
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
        logical function dataTypeEqual (d1, d2)
            use m
            type (dataType(4)), intent(in) :: d1, d2 ! tcx: (4)
        end function
    end interface
end module

program ffinal514a18_1k
use m
use m1, only: child
use m2
    type (child(4)), target :: c1 ! tcx: (4)

    if (d1_m == makeData (1)) then
        print *, 'error occurs'
        error stop 101_4
    end if

    print *, ''


    if (d1_m == dataType(4)(c1)) then ! tcx: (4)
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


logical function dataTypeEqual (d1, d2)
    use m
    type (dataType(4)), intent(in) :: d1, d2 ! tcx: (4)

    if ((.not. associated(d1%data)) .and. (.not. associated(d2%data))) then
        dataTypeEqual = .true.
    else if (associated (d1%data) .and. associated(d2%data)) then
        dataTypeEqual = (d1%data%id == d2%data%id)
    else
        dataTypeEqual = .false.
    end if
end function


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 1 changes
! type: dataType - added parameters (kdataType_1) to invoke with (4) / declare with (4) - 9 changes
! type: child - added parameters () to invoke with (4) / declare with (4) - 3 changes
