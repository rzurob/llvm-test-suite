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
!*  DATE                       : 04/25/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous item (new_line() during the
!                               stream output behaves as if a / edit descript)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fmisc037
    open (1, access='stream', form='formatted', file='fmisc037.data')

    write (1, '(2a)', advance='no') 'start', new_line('a')  !<-- create a new record by the write
    write (1, *) 'test'     !<-- should have a leading space

    !! same test for the following two statements
    write (1, '(a)', advance='no') 'again'//new_line('a')
    write (1, *) 'test'

    close (1, status = 'keep')
end
