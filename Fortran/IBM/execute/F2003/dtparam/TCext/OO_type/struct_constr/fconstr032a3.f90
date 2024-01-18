! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr032a3.f
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
! %GROUP: fconstr032a3.f
! %VERIFY: fconstr032a3.out:fconstr032a3.vf
! %STDIN:
! %STDOUT: fconstr032a3.out
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
!*  DATE                       : 10/14/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : STRUCTURE CONSTRUCTOR (polymorphic allocatable
!                               components in structure constructor)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = 1

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'no-name'

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

module m1
use m
    type container(k3,n2)    ! (4,20)
        integer, kind                :: k3
        integer, len                 :: n2
        class(base(k3)), allocatable :: data(:)
    end type
end module

program fconstr032a3
use m1
    type (container(4,20)) :: co1

    class (base(4)), allocatable :: b1(:)
    class (base(4)), allocatable :: c1(:)

    allocate (b1(2:3), source = (/child(4,1,20)(2,'test2'), child(4,1,20)(3,'test3')/))

    co1 = container(4,20) (b1)

    if (size (co1%data) /= 2) error stop 1_4

    if ((lbound(co1%data,1) /= 2) .or. (ubound (co1%data, 1) /= 3)) error stop 2_4

    call co1%data(2)%print
    call co1%data(3)%print

    co1 = container(4,20) (c1)

    if (allocated (co1%data)) error stop 3_4

    deallocate (b1)

    co1 = container(4,20) (b1)

    if (allocated (co1%data)) error stop 4_4
end
