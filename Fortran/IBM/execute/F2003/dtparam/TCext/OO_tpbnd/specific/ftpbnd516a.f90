! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp -qreuse=base /tstdev/OO_tpbnd/specific/ftpbnd516a.f
! opt variations: -qnock -qnol -qnodeferredlp -qreuse=none

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
! %GROUP: ftpbnd516a.f
! %VERIFY: ftpbnd516a.out:ftpbnd516a.vf
! %STDIN:
! %STDOUT: ftpbnd516a.out
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
!*  DATE                       : 07/05/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : specific type bound (type-bound function
!                               returns structure with allocatable components)
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id

        contains

        procedure :: makeArray => createBaseArray
        final :: finalizeBase, finalizeBaseRank1
        procedure :: print => printBase
    end type

    type, extends (base) :: child(k2)    ! (20,4,1)
        integer, kind             :: k2
        character(kind=k2,len=n1) :: name

        contains

        procedure :: makeArray => createChildArray
        final :: finalizeChild, finalizeChildRank1
        procedure :: print => printChild
    end type

    type (base(20,4)) :: b1_m
    type (child(20,4,1)) :: c1_m (2)

    contains

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(*,4,1)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    function createBaseArray (b, n)
        class (base(*,4)), intent(in) :: b
        integer*4, intent(in) :: n
        class (base(:,4)), allocatable :: createBaseArray (:)

        allocate (base(20,4) :: createBaseArray(n))

        createBaseArray%id = b%id
    end function

    function createChildArray (b, n)
        class (child(*,4,1)), intent(in) :: b
        integer*4, intent(in) :: n
        class (base(:,4)), allocatable :: createChildArray (:)

        allocate (createChildArray(n), source=b)
    end function

    subroutine finalizeBase (b)
        type (base(*,4)) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type(base(*,4)) :: b(:)

        print *, 'finalizeBaseRank1'
    end subroutine

    subroutine finalizeChild (c)
        type (child(*,4,1)) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildRank1 (c)
        type (child(*,4,1)) :: c(:)

        print *, 'finalizeChildRank1'
    end subroutine

    subroutine printData (b)
        class (base(:,4)), allocatable, intent(in) :: b(:)

        if (allocated (b)) then
            do i = 1, size (b)
                call b(i)%print
            end do
        else
            print *, 'the input is not allocated'
        end if
    end subroutine
end module

program ftpbnd516a
use m
    class (base(:,4)), allocatable :: b1 (:)

    call printData (b1)

    b1_m%id = 10

    c1_m%id = (/20, 30/)
    c1_m%name = (/'c1_m_20', 'c1_m_30'/)

    print *, size (b1_m%makeArray (3))
    print *, size (c1_m(1)%makeArray (2))

    call printData (b1_m%makeArray (3))

    call printData (c1_m(1)%makeArray (5))
    call printData (c1_m(2)%makeArray (3))

    print *, 'end'
end
