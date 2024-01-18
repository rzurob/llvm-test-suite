! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all /tstdev/OO_poly/dummy_arg/fArg013a6.f
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
! %GROUP: fArg013a6.f
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
!*  DATE                       : 12/14/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (TARGET attribute of
!                               dummy-arg and pointer assignment)
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
    end type

    type, extends(base) :: child(k2)    ! (4,20,1)
        integer, kind             :: k2
        integer(k1)               :: id
        character(kind=k2,len=n1) :: name
    end type

    type container(k3,n2)    ! (4,20)
        integer, kind               :: k3
        integer, len                :: n2
        class(base(k3,n2)), pointer :: data (:) => null()
    end type

    class (container(4,20)), allocatable :: co1 (:)

    contains

    subroutine test1 (b)
        class (base(4,*)), target, intent(in) :: b (10:)

        !! assign the last element in co1 to input
        co1(ubound(co1,1))%data => b
    end subroutine
end module

program fArg013a6
use m
    type (child(4,20,1)) :: c1 (4)

    c1 = (/child(4,20,1)(1,'c1_1'), child(4,20,1)(2,'c1_2'), child(4,20,1)(3,'c1_3'), child(4,20,1)(4,'c1_4')/)

    allocate (co1(0:5))

    allocate (co1(0)%data(0:3), source=c1)

    call test1 (co1(0)%data)

    if (.not. associated (co1(0)%data, co1(5)%data)) error stop 1_4

    if ((lbound (co1(5)%data, 1) /= 10) .or. (ubound(co1(5)%data, 1) /= 13)) &
            error stop 2_4
end
