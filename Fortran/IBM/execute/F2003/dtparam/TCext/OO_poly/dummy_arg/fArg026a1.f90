! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg026a1.f
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
! %GROUP: fArg026a1.f
! %VERIFY: fArg026a1.out:fArg026a1.vf
! %STDIN:
! %STDOUT: fArg026a1.out
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
!*  DATE                       : 06/04/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (dummy-procedure as
!*                               the actual argument; subroutine with implicit
!*                               interface)
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
        integer(k1)   :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

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

    subroutine modifyBase (b, sub)
        class (base(4)), intent(inout) :: b (:)

        call sub (b)
    end subroutine
end module

program fArg026a1
use m
    external increaseID4OddIdx

    type (child(4,1,20)) :: c1 (10)

    class (base(4)), pointer :: b1 (:)

    allocate (b1(20))

    b1%id = (/(i,i=1,20)/)

    call modifyBase (b1, increaseID4OddIdx)

    call modifyBase (b1(11:20), increaseID4OddIdx)

    do i = 1, 20
        call b1(i)%print
    end do

    c1 = (/(child(4,1,20)(i, 'c1_'//char(ichar('0')+i-1)), i=1,10)/)

    call modifyBase (c1(10:1:-1), increaseID4OddIdx)

    do i = 1, 10
        call c1(i)%print
    end do

    deallocate (b1)
end

subroutine increaseID4OddIdx (b)
use m
    type (base(4)), intent(inout) :: b(10)

    b(::2)%id = b(::2)%id + 100
end subroutine
