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
!*  DATE                       : 06/21/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : select type (test that spaces are optional for
!                              'select type' or 'end select'
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fselTyp514

    call test1 ((/1,2,3,4,5/))

    call test1 ((/1.5, 2.3/))

    contains

    subroutine test1 (x)
        class (*), intent(in) :: x(:)

        selectType (x)
            type is (integer)
                if (size(x) /= 5) error stop 1_4
                if (any(x /= (/1,2,3,4,5/))) error stop 2_4
        endSelect
    end subroutine
end
