! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg025a_1.f
!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg025a_1.f
! %VERIFY: fArg025a_1.out:fArg025a_1.vf
! %STDIN:
! %STDOUT: fArg025a_1.out
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 07/22/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (dummy-procedure in the
!                               argument association)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    interface
        function findAlg (b1, b2)
        import base
            class (base(4)), pointer :: findAlg (:)
            class (base(4)), intent(in) :: b1 (:), b2
        end function
    end interface

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine


    subroutine printMatches (b1, func, b2)
        class (base(4)), intent (in) :: b1(:), b2
        procedure (findAlg) func

        class (base(4)), pointer :: temp(:)

        temp => func (b1, b2)

        if (associated (temp)) then
            do i = lbound(temp,1), ubound(temp, 1)
                call temp(i)%print
            end do

            deallocate (temp)
        else
            print *, 'there is no data with matching ID'
        end if
    end subroutine
end module


program fArg025a_1
use m
    procedure (findAlg) :: find1

    type(child(4,1,20)) :: c1 (10)

    c1 = (/(child(4,1,20)(mod(i,4), 'c1_'//char(ichar('0')+i-1)), i=1,10)/)

    call printMatches (c1, find1, base(4)(3))

    print *, 'second print'

    call printMatches (c1(10:1:-1), find1, child(4,1,20)(2, 'test'))
end


!! to be more realistic generic type bound support is needed as well as the
!SELECT TYPE construct.
function find1 (b1, b2)
use m, only: base, child
    class (base(4)), pointer :: find1 (:)
    class (base(4)), intent(in) :: b1 (:), b2

    integer*4 matchSize, indexs(size(b1))

    nullify (find1)

    matchSize = 0
    indexs = 0

    do i = 1, size(b1)
        if (b1(i)%id == b2%id) then    ! then we find a match based on id
            matchSize = matchSize + 1
            indexs(matchSize) = i
        end if
    end do

    if (matchSize /= 0) then
        allocate (find1(matchSize), source=b1(indexs(1:matchSize)))
    end if
end function

