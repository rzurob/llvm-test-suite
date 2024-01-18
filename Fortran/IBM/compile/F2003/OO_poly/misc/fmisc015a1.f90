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
!*  DATE                       : 03/17/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous item (defect 296753)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fmisc015a1
    integer, pointer :: i2

    associate (x => 100)
        x = 1                       !<-- illegal
        read (*, *, iostat = x) i1  !<-- illegal

        allocate (i2, stat=x)       !<-- illegal

        do x = 1, 2                 !<-- illegal
            print *, 'abc'
        end do

        associate (y => x)
            y = 2                   !<-- this is illegal too
        end associate
    end associate
end
