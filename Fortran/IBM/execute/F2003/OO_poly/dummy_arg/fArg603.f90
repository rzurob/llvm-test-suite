!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg603.f
! %VERIFY: fArg603.out:fArg603.vf
! %STDIN:
! %STDOUT: fArg603.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
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

program fArg603
    integer*4, pointer :: i(:)
    integer*4, target :: i1 (100), i2(10)

    i1 = (/(j, j=1,100)/)
    i2 = (/1, 11, 21, 31, 41, 51, 61, 71, 81, 91/)

    i => i1

    call abc (i)
    call abc(i(i2))
    call abc(i(::2))

    print *, sizeTeller (i)
    print *, sizeTeller (i(::2))
    print *, sizeTeller (i(i2))


    contains

    subroutine abc(p)
        class(*), intent(in) :: p(:)

        print *, size(p)
    end subroutine

    integer*4 function sizeTeller (p)
        class (*), intent(in) :: p(:)

        sizeTeller = size(p)
    end function
end
