! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr025.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr025.f
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
!*                               components initialization)
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

    type, extends(base) :: child(k3,n3)    ! (4,20,1,20)
        integer, kind             :: k3
        integer, len              :: n3
        character(kind=k3,len=n3) :: name
    end type

    type, extends(dataType) :: moduleData(k4)    ! (4,20,4)
        integer, kind :: k4
        integer(k4)   :: id
    end type

    type (child(4,20,1,20)), save :: c1_m, c2_m
    type (base(4,20)), save :: b1_m, b2_m
    type (moduleData(4,20,4)), target :: m1_m = moduleData(4,20,4) (1)
    type (moduleData(4,20,4)), target :: m2_m
    type (dataType(4,20)), target :: d1_m

    contains

    subroutine initializeModData
        b1_m = base(4,20) (m1_m)

        m2_m = moduleData(4,20,4)(id = 2)
        b2_m = base(4,20) (data = m2_m)

        c1_m = child(4,20,1,20) (m1_m, 'c1_m')

        d1_m = dataType(4,20)()
        c2_m = child(4,20,1,20) (name = 'c2_m', data = d1_m)
    end subroutine
end module

program fconstr025
use m1

    type, extends(dataType) :: mainData(k5)    ! (4,20,8)
        integer, kind :: k5
        integer(k5)   :: value
    end type

    type (mainData(4,20,8)), target :: md1 = mainData(4,20,8) (100)

    type (base(4,20)) :: b1, b2
    type (child(4,20,1,20)) :: c1, c2

    type (mainData(4,20,8)), target :: md10 = mainData(4,20,8) (5)
    type (mainData(4,20,8)), target :: md11 = mainData(4,20,8) (-1)

    ! when the program started, all the pointer components are dissassociated
    ! for b1, b2, c1, c2, b1_m, b2_m, c1_m and c2_m

    if (associated (b1%data) .or. associated (b2%data) .or. &
        associated (c1%data) .or. associated (c2%data) .or. &
        associated (b1_m%data) .or. associated (b2_m%data) .or. &
        associated (c2_m%data) .or. associated (c2_m%data) ) &
                error stop 20_4

    ! initialize all data variables

    b1 = base(4,20) (md10)
    b2 = base(4,20) (data = m1_m)

    c1 = child(4,20,1,20) (data = md1, name = 'c1')
    c2 = child(4,20,1,20) (name = 'c2', data = md11)

    call initializeModData

    ! validate all the main program variables b1, b2, c1 and c2
    if (.not. associated (b1%data, md10)) error stop 1_4

    if (.not. associated (b2%data, m1_m)) error stop 2_4

    if ((.not. associated (c1%data, md1)) .or. (c1%name /= 'c1')) error stop 3_4

    if ((.not. associated (c2%data,md11)) .or. (c2%name /= 'c2')) error stop 4_4


    ! validate the module variables: b1_m, b2_m, c1_m and c2_m
    if (.not. associated (b1_m%data, m1_m)) error stop 5_4

    if (.not. associated (b2_m%data, m2_m)) error stop 6_4

    if ((.not. associated(c1_m%data,m1_m)) .or. (c1_m%name/='c1_m')) error stop 7_4

    if ((.not. associated(c2_m%data)).or.(c2_m%name /= 'c2_m')) error stop 8_4

end
