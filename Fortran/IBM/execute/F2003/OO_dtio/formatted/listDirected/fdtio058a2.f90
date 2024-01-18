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
! %GROUP: fdtio058a2.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f fdtio058.2.o fdtio058.1.o libtemp.a m.mod
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
!*  DATE                       :
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                :
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

program fdtio058a2
use m
    type, extends(base) :: child
        character(20) :: name
    end type

    class (child), allocatable :: c1(:)

    integer stat1
    character(200) error

    allocate (c1(2))

    c1%name = (/'test 1', 'test 2'/)

    open (1, file='fdtio058a2.data')

    write (1, *) 'val1=', -100_8
    write (1, *) 'val2=', 100_8

    rewind 1

    read (1, *, iostat = stat1, iomsg = error) c1

    if (stat1 /= 0) then
        print *, stat1, error
        error stop 1_4
    end if

    !! verify the results
    if ((.not. allocated (c1(1)%i1)) .or. (.not. allocated (c1(2)%i1))) &
                error stop 2_4


    if (any (c1%name /= (/'test 1', 'test 2'/))) error stop 3_4

    if ((c1(1)%i1 /= -100) .or. (c1(2)%i1 /= 100)) error stop 4_4

    close(1, status='delete')
end
