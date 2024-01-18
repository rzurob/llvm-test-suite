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
!*  DATE                       : 06/15/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 311904)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    character(20) :: s

    s = 'test2'

    call abc ('xlftest')
    call abc2 (s)

    contains

    subroutine abc (s)
        character, intent(in) :: s(4)

        if (s(1) /= 'x') error stop 1_4
        if (s(2) /= 'l') error stop 2_4
        if (s(3) /= 'f') error stop 3_4
        if (s(4) /= 't') error stop 4_4
    end subroutine

    subroutine abc2 (s)
        character, intent(in) :: s(*)

        if (any(s(1:5) /= (/'t','e','s','t','2'/))) error stop 5_4

        if (any(s(6:20) /= ' ')) error stop 6_4
    end subroutine
end


