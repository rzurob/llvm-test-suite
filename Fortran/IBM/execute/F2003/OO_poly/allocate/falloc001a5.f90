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
! %GROUP: falloc001a5.f
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
!*  DESCRIPTION                : ALLOCATE (kind() and len() in type-spec for
!                               intrinsic types in ALLOCATE statement; use
!                               integer and character types)
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

program falloc001a5
    class (*), pointer :: x1, x2, x3(:), x4(:,:), x5(:,:,:)

    integer(8), pointer :: i1
    character(10), allocatable :: ch1(:), ch2, ch3(:,:)

    character(100), parameter :: c_const = &
    'AAAAAAAAAABBBBBBBBBBCCCCCCCCCCDDDDDDDDDDEEEEEEEEEEFFFFFFFFFFGGGGGGGGGGHHHHHHHHHHIIIIIIIIIIJJJJJJJJJJ'

    allocate (integer(kind(10_8)) :: i1, x5(2:2,3:4, -1:0))

    allocate (character(10, kind('A')) :: ch1(0:9), x1)

    allocate (character(kind=kind('A'), len=10) :: ch2)

    allocate (character(len=len('0123456789'), kind = kind('A')) ::&
                    ch3(1,2), x2)


    i1 = 2_8**32*2

    ch1 = (/(repeat(char(ichar('A')+i), 10), i=0,9)/)

    ch2 = 'xlftest'

    ch3 = reshape ((/'test 101', 'test 202'/), (/1,2/))

    if (i1 /= 8589934592_8) error stop 1_4

    do i = 0, 9
        if (ch1(i) /= c_const(10*i+1:10*i+10)) error stop 2_4
    end do

    if (ch2 /= 'xlftest   ') error stop 3_4

    if ((ch3(1,2) /= 'test 202') .or. (ch3(1,1) /= 'test 101')) error stop 4_4
end
