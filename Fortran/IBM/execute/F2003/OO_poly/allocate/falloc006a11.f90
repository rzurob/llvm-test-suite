!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc006a11.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/17/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : allocate (function reference as source-expr for
!                               unlimited poly allocatable array)
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

module m
    contains

    integer(8) function createIDs (id, isize)
        integer (4), intent(in) :: id, isize

        dimension createIDs (isize)

        createIDs = (/(i, i=id, id+isize-1)/)
    end function
end module

program falloc006a11
use m
    integer(4) :: isize = 10
    class (*), allocatable :: x1(:)

    allocate (x1(isize), source=createIDs(100, isize))

    select type (x1)
        type is (integer(8))
            if (any (x1 /= (/(i, i=100, 109)/))) error stop 1_4
        class default
            error stop 2_4
    end select
end
