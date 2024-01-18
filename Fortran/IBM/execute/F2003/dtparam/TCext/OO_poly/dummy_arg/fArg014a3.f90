! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg014a3.f
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
! %GROUP: fArg014a3.f
! %VERIFY: fArg014a3.out:fArg014a3.vf
! %STDIN:
! %STDOUT: fArg014a3.out
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
!*  DATE                       : 11/15/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (assumed-size arrays in
!                               select type construct)
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
    type base(k1)    ! (8)
        integer, kind :: k1
        integer(k1)   :: id
    end type

    type, extends(base) :: child(k2,n1,k3)    ! (8,1,15,4)
        integer, kind             :: k2,k3
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
        logical(k3)               :: isSet
    end type

    contains

    subroutine test1 (a, b)
        class (base(8)), intent(in) :: a(*), b(2,*)

        select type (a)
            type is (base(8))
                print *, a(1), a(2)
            type is (child(8,1,*,4))
                print *, a(2), a(3)
            class default
                error stop 1_4
        end select

        select type (b)
            type is (base(8))
                print *, b(1,1), b(2,1)
            type is (child(8,1,*,4))
                print *, b(2,1), b(1,2)
            class default
                error stop 2_4
        end select
    end subroutine
end module

program fArg014a3
use m
    class (base(8)), allocatable :: b1(:,:), b2(:,:,:)
    character(2) :: c1(4) = (/'01', '02', '03', '04'/)

    allocate (b1(0:1,2), source=reshape((/(child(8,1,15,4)(i, name='xlftest_'//c1(i), &
                isSet=mod(i,2)==0), i=1,4)/), (/2,2/)))

    allocate (b2(2,2,0:1))

    b2%id = reshape ((/(j, j=1,8)/), (/2,2,2/))

    call test1 (b1, b2)
end
