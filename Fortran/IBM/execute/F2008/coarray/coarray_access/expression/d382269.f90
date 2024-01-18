! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 2010-10-08
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : defect 382269
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
    use ISO_FORTRAN_ENV
    implicit none
    real, save :: x(12)[*]
    integer me, np, i
    logical, external :: precision_r4

    me = this_image()
    np = num_images()

    call update (x, [(i + me*3 -3, i = 1, 3)]*1.0)
    sync all

    if (me ==1) then
        print *, x(:3*min(4, np))
        write (ERROR_UNIT, *) (x(i), i = 1, 3*min(4, np))

        do i = 1, 3*min(4, np)
            if (.not. precision_r4(x(i), i*1.0)) then
                print *, x(i), 'vs', i*1.0
                error stop 1
            end if
        end do
    end if

    contains

    subroutine update (y, val)
        real, intent(inout) :: y(*)[*]
        real, intent(in) :: val(3)

        integer begin

        begin = (me-1)*3 + 1

        if (me < 5) y(begin:begin+2)[1] = val
    end subroutine
    end
