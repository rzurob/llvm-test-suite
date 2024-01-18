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
! %GROUP: falloc001a.f
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
!*  DATE                       : 07/20/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : ALLOCATE (type-spec in ALLOCATE statement)
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

program falloc001a
    class (*), pointer :: x1
    class (*), allocatable :: x2, x3 (:)
    integer error
    integer(kind=8), pointer :: i1, i2(:), i3(:,:)
    complex(kind=8), allocatable :: cx1, cx2(:)

    logical precision_x6

    !! this statement is only for syntax check; SELECT TYPE construct is not
    !available yet.
    allocate (integer(kind=4) :: x1, x2, x3(3), stat=error)

    if (error /= 0) error stop 1_4

    if (size(x3) /= 3) error stop 2_4

    deallocate (x1, x2, x3)

    !! similarly the following 3 statements are for syntax check only

    allocate (complex*8 :: x1)
    allocate (integer :: x2)
    allocate (complex(kind = 4) :: x3(2:3))

    if ((lbound(x3,1) /= 2) .or. (ubound(x3,1) /= 3)) error stop 3_4

    !! the following 2 allocate statements are checked
    allocate (integer(8) :: i1, i2(2:3), i3(3, -1:0))

    allocate (complex*16 :: cx1, cx2(100:101))

    i1 = 100_8
    i2 = (/2_8, 3_8/)

    i3(:,-1) = (/3_8, 2_8, 1_8/)
    i3(:,0) = i3(3:1:-1,-1)

    if ((i1 /= 100) .or. any (i2 /= (/2, 3/))) error stop 4_4

    if (any (reshape (i3, (/6/)) /= (/3,2,1,1,2,3/))) error stop 5_4

    cx1 = (1.d1, 1.d0)

    cx2 = (/cx1, (2.d1, 2.d0)/)

    if (.not. precision_x6 (cx1, (10.d0, 1.d0))) error stop 6_4

    if (.not. precision_x6 (cx2(100), cx1))  error stop 7_4
    if (.not. precision_x6 (cx2(101), (20.d0, 2.d0))) error stop 8_4
end
