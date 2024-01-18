!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 05/16/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : class keyword (unlimited poly data in IO; use
!                               namelist)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fclass002d1
    class(*), pointer :: x1
    namelist /bad1/ x1, i1

    write (1, bad1)

    contains

    subroutine test (x)
        class (*), intent(in) :: x(:)

        namelist /bad2/ x, x

        write (1, bad2)
    end subroutine
end
