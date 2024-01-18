! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr029.f
! with manual adjustment to original (moving def of base after childData)
!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr029.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*                                                                     
!*  TEST CASE TITLE            :
!*                                                                     
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 12/23/2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : structure constructor (polymorphic pointer
!*                               array component)
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

    type, extends(dataType) :: childData(k2)    ! (4,20,4)
        integer, kind :: k2
        integer(k2)   :: id
    end type

    type, extends(childData) :: mData(k3,n2)    ! (4,20,4,1,20)
        integer, kind             :: k3
        integer, len              :: n2
        character(kind=k3,len=n2) :: name
    end type

    type base(k4,n3)    ! (4,20)
        integer, kind                       :: k4
        integer, len                        :: n3
        class(childData(k4,n3,k4)), pointer :: value(:) => null()
    end type

    type (base(4,20)), save :: b1_m(3), b2_m

    class (childData(4,20,4)), pointer :: cd1_mptr(:) => null()
    type (childData(4,20,4)), target :: cd1_m(10)
    type (mData(4,20,4,1,20)), target :: md1_m(20)

    contains

    subroutine initializeModuleData
        cd1_m = (/(childData(4,20,4) (i), i=11,20)/)

        md1_m = (/(mData(4,20,4,1,20)(i, name='md1_m'), i=101, 120)/)

        b1_m = base(4,20) (value = cd1_m)

        b2_m = base(4,20) (cd1_mptr)
        b2_m = base(4,20) (value = md1_m)
    end subroutine
end module

program fconstr029
use m

    type, extends(base) :: child(k5,n4)    ! (4,20,1,20)
        integer, kind             :: k5
        integer, len              :: n4
        character(kind=k5,len=n4) :: name
    end type

    type (base(4,20)) :: b1 = base(4,20) (null())
    type (base(4,20)) :: b2

    type (child(4,20,1,20)) :: c1

    type (childData(4,20,4)), target :: cd1(5)
    type (mData(4,20,4,1,20)), target :: md1(6)

    ! at this point, all pointer components are nulls
    if (associated (b1%value) .or. associated (b2%value) .or. &
        associated (b1_m(1)%value) .or. associated (b1_m(2)%value) .or. &
        associated (b1_m(3)%value) .or. associated (b2_m%value) .or. &
        associated (c1%value)) error stop 20_4

    call initializeModuleData

    cd1 = (/(childData(4,20,4)(i+1), i=1,5)/)
    md1 = (/(mData(4,20,4,1,20)(2*i, 'md1'), i=31,36)/)


    b1 = base(4,20) (md1)
    b2 = base(4,20) (value = cd1)

    c1 = child(4,20,1,20) (base = base(4,20) (value = cd1), name = 'c1')

    ! validate all the data: b1, b2, b1_m, b2_m and c1

    if ((.not. associated(b1%value, md1)) .or. (size(b1%value) /= 6)) error stop 1_4

    do i = 1,6
        if (b1%value(i)%id /= 2*i+60) error stop 2_4
    end do


    if ((.not. associated (b2%value,cd1)) .or. (size(b2%value) /= 5)) error stop 3_4

    do i = 1, 5
        if (b2%value(i)%id /= i+1) error stop 4_4
    end do


    if (.not. associated (c1%value, cd1) .or. (c1%name /= 'c1')) error stop 5_4
    if (size(c1%value) /= size(cd1)) error stop 6_4


    do i = 1, size(b1_m)
        if ((.not. associated (b1_m(i)%value, cd1_m)) .or. &
            (size(b1_m(i)%value) /= 10)) error stop 7_4

        do j = 1, 10
            if (b1_m(i)%value(j)%id /= 10+j) error stop 8_4
        end do
    end do


    if ((.not. associated (b2_m%value, md1_m)) .or. &
        (size(b2_m%value) /= 20)) error stop 9_4

    do i = 1, 20
        if (b2_m%value(i)%id /= 100+i) error stop 10_4
    end do


    !! reset some of the variables
    nullify (cd1_mptr)

    c1 = child(4,20,1,20) (cd1_mptr, 'c1')
    if (associated (c1%value)) error stop 11_4

    cd1_mptr => md1_m
    c1 = child(4,20,1,20) (cd1_mptr, 'c1')

    if (.not. associated (c1%value, md1_m)) error stop 12_4
    if (size (c1%value) /= 20) error stop 13_4

    do i = 101, 120
        if (c1%value(i-100)%id /= i) error stop 14_4
    end do
end
