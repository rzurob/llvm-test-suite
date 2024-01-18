! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc016a.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc016a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (SAVE attribute on allocatable
!                               variables)
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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        INTEGER(8) :: id
    end type

    type, extends(base) :: child(k2,n2)    ! (4,20,1,15)
        integer, kind             :: k2
        integer, len              :: n2
        character(kind=k2,len=n2) :: name
    end type
end module

subroutine test1
use m
    type (base(4,20)), allocatable, save :: b1, b2(:)
    class (base(4,20)), allocatable, save :: b3, b4(:)

    integer(4), save :: timesVisited = 0


    if (allocated (b1))  deallocate (b1)
    if (allocated (b2))  deallocate (b2)
    if (allocated (b3))  deallocate (b3)
    if (allocated (b4))  deallocate (b4)


    if (mod (timesVisited,2) == 0) then
        allocate(b1, b2(timesVisited))
        allocate(child(4,20,1,15) :: b3, b4(timesVisited))
    end if
end subroutine

program falloc016a
    do i = 1, 5
        call test1
    end do
end
