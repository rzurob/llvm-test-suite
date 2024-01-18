! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr032.f
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
! %GROUP: fconstr032.f
! %VERIFY: fconstr032.out:fconstr032.vf
! %STDIN:
! %STDOUT: fconstr032.out
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
!*  DATE                       : 04/28/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : structure constructor (poly-allocatable
!*                               component in structure constructor)
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

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2)    ! (4,20,4)
        integer, kind :: k2
        integer(k2)   :: id = 1

        contains

        procedure, nopass :: typeID => childTypeID
        procedure :: print => printChild
    end type

    contains

    integer*4 function baseTypeID ()
        baseTypeID = 1
    end function

    integer*4 function childTypeID ()
        childTypeID = 2
    end function

    subroutine printChild (b)
        class (child(4,*,4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printBase (b)
        class (base(4,*)), intent(in) :: b

        print *, 'empty type'
    end subroutine
end module

module m1
use m
    type container(k3,n2)    ! (4,20)
        integer, kind                   :: k3
        integer, len                    :: n2
        class(base(k3,n2)), allocatable :: component
    end type
end module

program fconstr032
use m1
    class (base(4,20)), allocatable :: b1
    type (container(4,20)) :: co1 = container(4,20)(null())

    if (allocated (co1%component)) error stop 1_4

    if (co1%component%typeID () /= 1) error stop 2_4

    allocate (child(4,20,4) :: b1)

    co1 = container(4,20) (component = b1)

    if (co1%component%typeID() /= 2) error stop 3_4

    call co1%component%print

    deallocate (b1)

    allocate (b1, source = child(4,20,4) (10))

    co1 = container(4,20) (b1)

    if (co1%component%typeID() /= 2) error stop 4_4

    call co1%component%print
end
