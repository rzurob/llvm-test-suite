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
! %GROUP: falloc005a22.f
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
!*  DATE                       : 07/27/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : ALLOCATE (transfer() in source-expr)
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

program falloc005a22
    complex(4), allocatable :: cx1(:)
    complex(8), allocatable :: cx2(:)

    character, allocatable :: ch1(:)

    logical precision_x8, precision_x6

    allocate (cx1(2), source=transfer((/1.1, 2.2, 3.3, 4.4/),&
                            (/(0.0, 0.0), (0.0, 0.0)/)))


    allocate (cx2(2:3), source=transfer((/1.1d0, 2.2d0, 3.3d0, 4.4d0/), &
                        (0.0d0, 0.0d0)))

    
    allocate (ch1(5), source=transfer((/65_1,66_1,67_1,68_1,69_1/), 'A', 5))

    if (.not. precision_x8 (cx1(1), (1.1e0,2.2e0))) error stop 1_4
    if (.not. precision_x8 (cx1(2), (.33e1, .44e1))) error stop 2_4

    if (.not. precision_x6 (cx2(2), (.11d1, .22d1))) error stop 3_4
    if (.not. precision_x6 (cx2(3), (.11d1, .22d1))) error stop 4_4

    if (any (ch1 /= (/(char(i), i=65,69)/))) error stop 5_4
end
