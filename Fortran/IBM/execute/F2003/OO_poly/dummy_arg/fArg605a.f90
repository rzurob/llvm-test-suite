!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/31/2005
!*
!*  DESCRIPTION                : argument association (intrinsic function return
!                               results as actual-arg to be associated with
!                               unlimited poly dummy-arg)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fArg605a
    integer i1(10)

    real(8) r1(10)


    i1 = (/(j, j = 1, 10)/)
    r1 = (/(j*1.1d0, j = 1, 10)/)

    call printX3 (cmplx(i1, kind=8))

    call printX3 (cmplx (r1(3::2), kind=4))

    contains

    subroutine printX3 (x)
        class(*), intent(in) :: x(*)

        select type (x)
            type is (complex(4))
                write (*, 100) x(1:3)
            type is (complex(8))
                write (*, 200) x(1:3)
            class default
                print *, 'others'
        end select

100 format (3(1x, '(', f10.2, ',', f10.2, ')'))
200 format (3(1x, '(', d10.2, ',', d10.2, ')'))
    end subroutine
end
