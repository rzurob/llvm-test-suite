! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-09-14
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - Combination of attributes CONTIGUOUS,
!*                                   POINTER and SAVE
!*                               - Target of array pointer is dummy arg of
!*                                   explicit array
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

program main

    integer  :: arr(20)

    arr = (/(i,i=1,20)/)

    call sub(arr)
    call sub(arr)

    if ( any(arr .ne. (/23,26,29,32,35,38,41,44,47,50,53,56,59,62,65,68,71,74,77,80/))) error stop 21

    contains

    subroutine sub (x)

        integer :: i, x(11:30)
        target x
        integer, pointer, contiguous, save :: z(:)

        if ( .not. associated(z)) then
            z=>x
        else
            if ( any(z .ne. (/12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50/))) error stop 11
        endif

        do i = 1+10, size(x)+10
            x(i) = i + x(i)
        end do

    end subroutine

end
