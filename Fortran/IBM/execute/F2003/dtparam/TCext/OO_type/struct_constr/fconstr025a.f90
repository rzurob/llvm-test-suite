! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr025a.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr025a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/22/2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (polymorphic pointer
!*                               initialization)
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
    end type
end module

module m1
use m
    type base(k2,n2)    ! (4,20)
        integer, kind                   :: k2
        integer, len                    :: n2
        class(dataType(k2,n2)), pointer :: data => null()
    end type

    type, extends(dataType) :: childData(k3)    ! (4,20,4)
        integer, kind :: k3
        integer(k3)   :: id
    end type

    type, extends(childData) :: mData(k4,n3)    ! (4,20,4,1,20)
        integer, kind             :: k4
        integer, len              :: n3
        character(kind=k4,len=n3) :: name
    end type

    type (childData(4,20,4)), target :: cd1_m
    type (mData(4,20,4,1,20)), target :: md1_m = mData(4,20,4,1,20) (name='md1_m', id = 10)
    type (base(4,20)), save :: b1_m, b2_m
end module

program fconstr025a
use m1

    type (base(4,20)) :: b1, b2
    type (mData(4,20,4,1,20)), target :: md1 = mData(4,20,4,1,20) (1, 'md1')
    type (childData(4,20,4)), target :: cd1 = childData(4,20,4) (id = 100)

    class (dataType(4,20)), pointer :: d_ptr

    d_ptr => md1
    b1 = base(4,20) (d_ptr)


    d_ptr => cd1
    b2 = base(4,20) (d_ptr)


    d_ptr => cd1_m
    b1_m = base(4,20) (d_ptr)


    d_ptr => md1_m
    b2_m = base(4,20) (d_ptr)


    ! validate data b1, b2, b1_m and b2_m
    if (.not. associated (b1%data, md1)) error stop 1_4

    if (.not. associated (b2%data, cd1)) error stop 2_4

    if (.not. associated (b1_m%data, cd1_m)) error stop 3_4

    if (.not. associated (b2_m%data, md1_m)) error stop 4_4
end
