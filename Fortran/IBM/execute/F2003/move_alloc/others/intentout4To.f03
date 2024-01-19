! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*                               TO is dummy arg with intent(out) attr
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


    real*8, allocatable :: TO(:), FROM(:)
    logical precision_r8

    allocate ( from(10), source = (/ (dble(i), i= 1,10) /) )

    call sub(to)

    if ( allocated(from) ) error stop 51
    if ( .not. allocated(to) ) error stop 53

    do i = 1, 10
        if ( .not. precision_r8(to(i), dble(i)) )  call zzrc(i)
    end do

    contains
        subroutine sub(to)
            double precision, intent(out), allocatable :: to(:)
            call move_alloc (from, to)
        end subroutine
    end
