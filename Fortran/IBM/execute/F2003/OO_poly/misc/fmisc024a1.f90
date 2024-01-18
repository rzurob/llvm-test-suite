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
!*  DATE                       : 05/24/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous items (defect 294679; part 3:
!                               pure function uses associate construct)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fmisc024a1
    interface
        pure logical function test1 (i)
            integer(8), intent(in) :: i(:)
        end function
    end interface

    integer(8) i2(1000)

    i2 = (/(i*10, i=1,1000)/)

    if (test1 ((/(i*1_8, i=1,100)/))) error stop 1_4

    if (.not. test1 (i2(::2))) error stop 2_4
end


pure logical function test1 (i)
    integer(8), intent(in) :: i(:)

    associate (x => i, x1 => test1)
        x1 = (sum(x) > 10000_8)
    end associate

end function
