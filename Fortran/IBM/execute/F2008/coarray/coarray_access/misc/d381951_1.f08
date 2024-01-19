! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-12-23
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : defect 381951
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
    real, save :: x(1024)[*]
    integer me

    me = this_image()

    if (num_images() < 4) then
        if (me == 1) print *, 'requires at least 4 images'
        error stop 1
    end if

    call update (x, [(i + me*3 -3, i = 1, 3)]*1.0)
    sync all

    if (me == 1) print *, x(:12)

    contains

    subroutine update (y, val)
        real, intent(inout) :: y(*)[*]
        real, intent(in) :: val(3)

        integer begin

        begin = (me-1)*3 + 1

        y(begin:begin+2)[1] = val
    end subroutine
    end

