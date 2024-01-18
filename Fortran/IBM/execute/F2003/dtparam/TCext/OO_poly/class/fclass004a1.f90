! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/OO_poly/class/fclass004a1.f
! opt variations: -qck -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self -qreuse=base

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
! %GROUP: fclass004a1.f
! %VERIFY: fclass004a1.out:fclass004a1.vf
! %STDIN:
! %STDOUT: fclass004a1.out
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
!*  DATE                       : 04/01/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : class keyword (intrinsic assignment, RHS can be
!*                               poly-entities; also contains allocatable
!*                               component)
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
    type dataType(n1,k1,k2)    ! (20,4,4)
        integer, kind :: k1,k2
        integer, len  :: n1
        integer(k1)      i1
        integer(k2)      i2

        contains

        procedure :: print => printDataType
    end type

    type, extends(dataType) :: mData(n2,k3)    ! (20,4,4,20,4)
        integer, kind :: k3
        integer, len  :: n2
        integer(k3)      i3

        contains

        procedure :: print => printmData
    end type
    
    type base(n3,k4)    ! (20,4)
        integer, kind                         :: k4
        integer, len                          :: n3
        class(dataType(:,k4,k4)), allocatable :: data
        integer(k4)                           :: id = 0

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k5,n4)    ! (20,4,4,15)
        integer, kind :: k5
        integer, len  :: n4
        character(n4) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        if (allocated (b%data)) then
            call b%data%print
            print *, 'id = ', b%id
        end if
    end subroutine

    subroutine printChild (b)
        class (child(*,4,4,*)), intent(in) :: b

        call b%base%print
        print *, b%name
    end subroutine

    subroutine printDataType (d)
        class (dataType(*,4,4)), intent(in) :: d

        print *, d%i1, d%i2
    end subroutine

    subroutine printMdata (d)
        class (mData(*,4,4,*,4)), intent(in) :: d

        print *, d%i1, d%i2, d%i3
    end subroutine
end module

program fclass004a1
use m
    type (base(20,4)) :: b1
    class (base(:,4)), pointer :: b2

    type (child(20,4,4,15)), target :: c1
    type (mData(20,4,4,20,4)) :: md1

    md1 = mData(20,4,4,20,4) (10, 11, 12)

    c1 = child(20,4,4,15) (null(), 20, 'c1')

    allocate (c1%data, source = md1)

    b2 => c1

    b1 = b2


    call b1%print

    call b2%print
end
