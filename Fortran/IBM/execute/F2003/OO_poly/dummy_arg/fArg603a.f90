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
! %GROUP: fArg603a.f
! %VERIFY: fArg603a.out:fArg603a.vf
! %STDIN:
! %STDOUT: fArg603a.out
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
!*  DATE                       : 11/11/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (unlimited poly-pointer
!                               array and array sections used as the actual
!                               arguments)
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

program fArg603a
    class(*), pointer :: i(:)
    integer*4, target :: i1 (100), i2(10)

    i1 = (/(j, j=1,100)/)
    i2 = (/1, 11, 21, 31, 41, 51, 61, 71, 81, 91/)

    i => i1

    call abc(i)
    call abc(i(i2))
    call abc(i(::2))

    contains

    subroutine abc(p)
        class(*), intent(in) :: p(:)

        select type (p)
            type is (integer*4)
                print *, p(3)
            class default
                error stop 1_4
        end select
    end subroutine
end
