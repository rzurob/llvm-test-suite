! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/OO_poly/point_assgn/fpAssgn004a4.f
! opt variations: -qck -qnok -qnol -qnodeferredlp -qreuse=base

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn004a4.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (poly pointer assigned
!*                               to function return results; arrays; test
!*                               bounds, size and dynamic types)
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id

        contains

        procedure, nopass :: typeID => baseID
    end type

    type, extends (base) :: child(k2,n2)    ! (20,4,4,20)
        integer, kind :: k2
        integer, len  :: n2
        character(n2) :: name

        contains

        procedure, nopass :: typeID => childID
    end type

    contains

    integer*4 function baseID()
        baseID = 1
    end function

    integer*4 function childID ()
        childID = 2
    end function
end module


program fpAssgn004a4
use m
    interface makeData
        function baseArray (i)
        use m
            type(base(:,4)), pointer :: baseArray(:)
            integer*4, intent(in) :: i
        end function

        function childArray (i, c)
        use m
            type(child(:,4,4,:)), pointer :: childArray(:)
            integer*4, intent(in) :: i
            character(*), intent(in) :: c
        end function
    end interface

    class (base(:,4)), pointer :: b_ptr(:)

    !! make an array of base type pointer
    b_ptr => makeData(10)

    if ((size(b_ptr) /= 21) .or. (lbound(b_ptr, 1) /= -10) &
        .or. (ubound(b_ptr, 1) /= 10)) error stop 1_4

    if (b_ptr%typeID() /= 1) error stop 2_4

    if (any (b_ptr%id /= 10)) error stop 3_4


    !! make a pointer array of child type
    b_ptr => makeData(-10, 'good')

    if ((size(b_ptr) /= 20) .or. (lbound(b_ptr, 1) /= 1) &
        .or. (ubound(b_ptr, 1) /= 20)) error stop 4_4


    if (b_ptr%typeID() /= 2) error stop 5_4

    if (any (b_ptr%id /= -10)) error stop 6_4

    deallocate (b_ptr)
end

function baseArray (i)
use m
    type(base(:,4)), pointer :: baseArray(:)
    integer*4, intent(in) :: i

    type(base(20,4)), static, target :: oneArray (-10:10)

    baseArray => oneArray
    baseArray = base(20,4) (i)
end function

function childArray (i, c)
use m
    type(child(20,4,4,20)), pointer :: childArray(:)
    integer*4, intent(in) :: i
    character(*), intent(in) :: c

    allocate (childArray(20))

    childArray = child(20,4,4,20) (i, c)

end function
