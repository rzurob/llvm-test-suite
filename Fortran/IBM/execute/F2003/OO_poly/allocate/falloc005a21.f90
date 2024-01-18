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
! %GROUP: falloc005a21.f
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
!*  DATE                       : 07/17/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : ALLOCATE (level-5 expression used in the
!                               source-expr; logical-ops)
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

program falloc005a21
    logical(8), pointer :: lp1(:), lp2(:), lp3(:,:)
    logical(8) :: l1 (10), l2(5)

    l1(::2) = .true._8
    l1(2::2) = .false._8

    l2 = (/.false._8, .true._8, .true._8, .false._8, .false._8/)

    allocate (lp1(3), source=l1(2::3) .and. l2((/1,2,1/)))

    allocate (lp2(3), source= l1(2::3) .eqv. l2((/1,2,1/)))

    allocate (lp3(2,2), source=reshape ((l2(:4) .or. l1((/9,2,7,1/))), (/2,2/)))

    if (any (lp1 .neqv. (/.false., .true., .false./))) error stop 1_4

    if (.not. all (lp2)) error stop 2_4

    if (.not. all (lp3)) error stop 3_4

    deallocate (lp1, lp2, lp3)
end
