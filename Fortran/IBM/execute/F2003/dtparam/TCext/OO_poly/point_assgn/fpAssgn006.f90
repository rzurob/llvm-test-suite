! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qnodeferredlp -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn006.f
! opt variations: -qck -qnok -qnol -qdefaultpv -qdeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/05/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (occurred for pointer
!*                               component during the intrinsic assignment)
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
    type dataType(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure, nopass :: print => printData
    end type

    type, extends(dataType) :: moduleData    ! (4,20)
        integer(k1) :: id

        contains
        procedure, nopass :: print => printMData
    end type

    contains

    subroutine printData
        print *, 'dataType'
    end subroutine

    subroutine printMData
        print *, 'moduleData'
    end subroutine
end module

module m1
use m

    type base(k2,n2)    ! (4,20)
        integer, kind                   :: k2
        integer, len                    :: n2
        class(dataType(k2,n2)), pointer :: data => null()
    end type

    type, extends(base) :: child    ! (4,20)
        character(n2) :: name
    end type
end module

program fpAssgn006
use m1
    type(base(4,20)) :: b1, b2
    type (child(4,20)) :: c1, c2

    type(dataType(4,20)), target :: d1
    type(moduleData(4,20)), target :: md1
    type(moduleData(4,20)), pointer :: md_ptr

    call b1%data%print
    call c1%data%print

    allocate (md_ptr)

    b1 = base(4,20) (data = d1)

    b2 = b1

    if (.not. associated (b2%data)) error stop 1_4

    if (associated (b2%data, d1)) error stop 10_4

    call b2%data%print

    c1 = child(4,20) (md1, 'c1')

    c2 = c1

    if (.not. associated (c2%data, md1)) error stop 2_4

    call c2%data%print

    md_ptr = moduleData(4,20) (100)

    c1 = child(4,20) (md_ptr, 'c1')
    c2 = c1

    if (.not. associated (c2%data, md_ptr)) error stop 3_4

    call c2%data%print
end
