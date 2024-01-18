! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc022a1.f
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
! %GROUP: falloc022a1.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
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
!*  DATE                       : 09/17/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : ALLOCATE (test on deallocation failure using a
!                               list of allocation
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

program falloc022a1
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id
    end type

    class (*), pointer :: x0, x1(:)

    class (*), target, allocatable :: y0, y1(:)
    integer stat1

    character(20) reason

    allocate (y0, source=base(4)(10))

    allocate (y1(-1:0), source= (/1, 10/))

    x1 => y1
    x0 => y1(0)

    !! this deallocate will succeed on y0 but fail on x1
    deallocate (x0, y0, x1, stat=stat1, errmsg=reason)

    if (stat1 /= 2) error stop 1_4

    if (allocated (y0) .or. (.not. allocated (y1))) error stop 2_4

    if (.not. associated (x1, y1)) error stop 3_4

    if (.not. associated (x0, y1(0))) error stop 4_4
end
