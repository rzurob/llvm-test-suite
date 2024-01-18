!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod && $TR_SRC/buildStatLib058.presh
! %COMPOPTS: -qfree=f90 -L./ -ltemp
! %GROUP: fdtio058.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f fdtio058.1.o fdtio058.2.o libtemp.a m.mod
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
!*  DATE                       : 12/02/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : DTIO generics (test the cases DTIO routines are
!                               built in library: static lib)
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

program fdtio058
use m
    class (base), pointer :: b1

    type (base), target :: b2

    integer stat
    character(200) err

    b1 => b2

    open (1, file='fdtio058.data')

    write (1, *, iostat=stat, iomsg=err) base (100)

    if (stat /= 0) then
        print *, stat, err
        error stop 1_4
    end if

    rewind 1

    read (1, *, iostat=stat, iomsg=err) b1

    if (stat /= 0) then
        print *, stat, err
        error stop 2_4
    end if

    if (.not. allocated (b2%i1)) error stop 3_4

    if (b2%i1 /= 100) error stop 4_4

    close (1, status='delete')
end
