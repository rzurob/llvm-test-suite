! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn007a5.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn007a5.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (poly-pointer
!*                               deallocated; then the dynamic type is declared
!*                               type; use array)
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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure, nopass :: typeID => baseTypeID
    end type

    type, extends (base) :: child    ! (4,20)
        integer(k1) :: id

        contains

        procedure, nopass :: typeID => childTypeID
    end type

    contains

    integer*4 function baseTypeID ()
        baseTypeID = 1
    end function

    integer*4 function childTypeID ()
        childTypeID = 2
    end function
end module

program fpAssgn007a5
use m
    class (base(4,:)), pointer :: b_ptr(:)

    class (child(4,20)), pointer :: c1(:)
    type (child(4,20)), pointer :: c2(:)

    b_ptr => null()
    nullify (c1, c2)

    if ((b_ptr%typeID() /= 1) .or. (c1%typeID() /= 2) .or. &
        (c2%typeID() /= 2)) error stop 1_4

    allocate (c1(10))

    b_ptr => c1

    if (b_ptr%typeID() /= 2) error stop 2_4

    deallocate (b_ptr)

    if (b_ptr%typeID() /= 1) error stop 3_4

    allocate (c2(100))

    b_ptr => c2

    if (b_ptr%typeID() /= 2) error stop 4_4

    deallocate (b_ptr)

    if (b_ptr%typeID() /= 1) error stop 5_4
end
