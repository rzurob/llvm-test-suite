! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodeferredlp -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn025a2.f
! opt variations: -qnok -qnol -qdeferredlp -qreuse=none

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn025a2.f
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
!*  DESCRIPTION                : data pointer assignment (pointer assignment
!*                               takes place during the intrinsic assignment for
!*                               the pointer component)
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
        class (*), pointer :: data => null()
    end type

    type, extends (dataType) :: mData    ! (4,20)
        integer(k1) :: id
    end type
end module

program fpAssgn025a2
use m
    class (mData(4,20)), target, allocatable :: md1
    type (mData(4,20)), pointer :: md2, md3

    integer*2, target :: i1 = 100

    allocate (md1, md2, md3)

    md2 = mData(4,20) (i1, id = 10)

    md3 = md2

    if (.not. associated (md3%data, i1)) error stop 1_4

    md1%dataType = md3%dataType

    if (.not. associated (md1%data, i1)) error stop 2_4

    md3 = mData(4,20) (id = 1, data = md1)

    md2 = md3

    if (.not. associated (md2%data, md1)) error stop 3_4

    deallocate (md2, md3)
end
