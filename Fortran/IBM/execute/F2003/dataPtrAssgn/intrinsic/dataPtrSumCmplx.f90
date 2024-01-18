!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION   use data-pointer as args of intrinsic SUM;
!*          data-pointer of type complex;
!*          data-target is array section;
!*            bounds-remapping&bound_spec ;
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    program main
        complex(4), target, allocatable :: t_cmplx(:)
        complex(4), pointer :: p_cmplx(:)

        logical, pointer :: lp(:)
        logical precision_x8

        allocate(lp(6))
        lp = (/ .true., .false., .true., .true., .false., .true. /)

        allocate(t_cmplx(20), source=(/( cmplx(i, i-1, 4), i=21,60,2 ) /) )

        ! bounds-remapping
        p_cmplx(3:8) => t_cmplx(::3)

        if ( .not. associated(p_cmplx) ) stop 5
        if ( lbound(p_cmplx,1) /= 3 ) stop 7
        if ( ubound(p_cmplx,1) /= 8 ) stop 9

        ! sum(array, mask)
        if(.not. precision_x8(sum(p_cmplx, lp),(144.0000000,140.0000000))) &
        stop 11

        deallocate (lp)
        allocate(lp(10))
        lp = (/ ( (10-i)>i, i=1,10 ) /)

        ! bounds-spec
        p_cmplx(0:) => t_cmplx
        p_cmplx(0:) => p_cmplx(10:)

        if ( lbound(p_cmplx, 1) /= 0 ) stop 13
        if ( ubound(p_cmplx, 1) /= 9 ) stop 15
        if ( .not. precision_x8(sum(p_cmplx, lp),(176.0000000,172.0000000))) &
        stop 17
    End program
