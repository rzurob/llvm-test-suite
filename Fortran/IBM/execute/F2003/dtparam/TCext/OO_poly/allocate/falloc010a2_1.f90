! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc010a2_1.f
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
! %GROUP: falloc010a2_1.f
! %VERIFY: falloc010a2_1.out:falloc010a2_1.vf
! %STDIN:
! %STDOUT: falloc010a2_1.out
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
!*  DATE                       : 12/14/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : ALLOCATE (check the bounds of the
!                               poly-allocatable components by structure
!                               constructor)
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
    type base(k1)    ! (8)
        integer, kind            :: k1
        integer(k1), allocatable :: ids(:, :)

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (8,1,17)
        integer, kind                          :: k2
        integer, len                           :: n1
        character(kind=k2,len=n1), allocatable :: names(:,:)

        contains

        procedure :: print => printChild
    end type

    type container(k3)    ! (8)
        integer, kind                :: k3
        class(base(k3)), allocatable :: data(:)
    end type

    contains

    subroutine printBase (b)
        class (base(8)), intent(in) :: b

        if (allocated (b%ids)) then
            print *, 'shape of ids:', shape(b%ids), '; lbound:', &
                    lbound(b%ids), '; ubound:', ubound(b%ids)

            print *, 'values of ids:', b%ids
        else
            print *, 'ids not allocated'
        end if
    end subroutine

    subroutine printChild (b)
        class (child(8,1,*)), intent(in) :: b

        print *, 'Child type:'
        call b%base%print

        if (allocated (b%names)) then
            print *, 'shape of names:', shape(b%names), '; lbound:', &
                    lbound(b%names), '; ubound:', ubound(b%names)

            print *, 'values of names:', b%names
        else
            print *, 'names not allocated'
        end if
    end subroutine
end module

program falloc010a2_1
use m
    class (base(8)), allocatable :: b1(:)
    class (container(8)), allocatable :: co1(:)
    type (container(8)), allocatable :: co2(:)

    type (child(8,1,17)) :: c1(2:3)
    integer(8) :: ids1(1,2), ids2(2:3,0:0)
    character(17) :: names1(0:0,2:3), names2(2,1)

    ids1(1,1) = 1000
    ids1(1,2) = 1001

    ids2(2,0) = 10000
    ids2(3,0) = 20000

    names1 (0,2) = 'xlf'
    names1 (0,3) = 'test'

    names2 (1,1) = 'xlftest'
    names2 (2,1) = 'testxlf'

    c1(2) = child(8,1,17)(ids1, names1)
    c1(3) = child(8,1,17)(ids2, names2)

    call c1(2)%print
    call c1(3)%print

end
