! GB DTP extension using:
! ftcx_dtp -qck -ql -qreuse=base /tstdev/OO_tpbnd/specific/ftpbnd515a1.f
! opt variations: -qnock -qnol -qreuse=none

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
! %GROUP: ftpbnd515a1.f
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
!*  DATE                       : 06/24/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (elemental binding call
!                               for a scalar and an array section)
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
    end type

    type, extends(base) :: child1(k2)    ! (20,4,1)
        integer, kind             :: k2
        character(kind=k2,len=n1) :: name

        contains

        procedure, pass (c1) :: compare => compareC1C2
    end type

    type, extends(base) :: child2(k3,n2)    ! (20,4,1,15)
        integer, kind             :: k3
        integer, len              :: n2
        character(kind=k3,len=n2) :: name

        contains

        procedure, pass (c2) :: compare => compareC1C2
    end type

    contains

    elemental logical function compareC1C2 (c1, c2)
        class (child1(*,4,1)), intent(in) :: c1
        class (child2(*,4,1,*)), intent(in) :: c2

        compareC1C2 = ((c1%name == c2%name) .and. (c1%id == c2%id))
    end function
end module

program ftpbnd515a1
use m
    type (child1(20,4,1)) :: c1(10)
    type (child2(20,4,1,15)) :: c2(2:11)

    logical verifyResult (5)

    c1 = (/(child1(20,4,1) (i, 'test'),i=1,10)/)

    c2 = (/(child2(20,4,1,15) (i, 'test'),i=1,10)/)

    !! check if a scalar can be used together with an array section
    verifyResult = .false.

    verifyResult (2) = .true.

    if (any(c1(2)%compare (c2(2:6)) .neqv. verifyResult)) error stop 1_4

    if (any(c2(5)%compare (c1(2::2)) .neqv. verifyResult)) error stop 2_4
end
