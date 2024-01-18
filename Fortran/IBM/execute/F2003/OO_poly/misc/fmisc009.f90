!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fmisc009.f
! %VERIFY: fmisc009.out:fmisc009.vf
! %STDIN:
! %STDOUT: fmisc009.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 287866)
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
    type base
        integer(4) :: id
    end type

    type retType
        integer(4) :: ind_val
        type (base) :: data
    end type

    interface
        function findAlg (b, id)
        import base, retType
            type (retType), pointer :: findAlg (:)
            type (base), intent(in) :: b(:)
            integer(4), intent(in) :: id
        end function
    end interface

    contains

    subroutine printMatches (b, alg, id)
        type (base), intent(in) :: b(:)
        procedure (findAlg) :: alg
        integer(4), intent(in) :: id

        type(retType), pointer :: temp (:)

        temp => alg (b, id)

        if (associated (temp)) then
            do i = lbound(temp,1), ubound(temp,1)
                print *, 'index:', temp(i)%ind_val, '; data:', temp(i)%data
            end do

            deallocate (temp)
        end if
    end subroutine
end module

program fmisc009
use m
    procedure (findAlg) :: find1

    type (base) :: b1 (20)

    b1%id = (/(mod(j,6),j=1,20)/)

    call printMatches (b1, find1, 5)
end

function find1 (b, id)
use m, only : base, retType
    type (retType), pointer :: find1(:)
    type (base), intent(in) :: b(:)
    integer(4), intent(in) :: id

    integer*4 matchSize, indexs(size(b))

    nullify (find1)

    matchSize = 0
    indexs = 0

    do i = 1, size(b)
        if (b(i)%id == id) then
            matchSize = matchSize + 1
            indexs(matchSize) = i
        end if
    end do

    if (matchSize /= 0) then
        allocate (find1(matchSize))

        find1%ind_val = indexs(1:matchSize)
        find1%data = b(indexs(1:matchSize))
    end if
end function
