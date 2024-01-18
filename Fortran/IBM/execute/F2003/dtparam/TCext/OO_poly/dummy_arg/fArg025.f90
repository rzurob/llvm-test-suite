! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg025.f
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
! %GROUP: fArg025.f
! %VERIFY: fArg025.out:fArg025.vf
! %STDIN:
! %STDOUT: fArg025.out
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
!*  DATE                       : 06/03/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (dummy procedure)
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
        integer function sortAlg (b)
        import base
            class (base(4)), intent(in) :: b(:)
            dimension sortAlg(size(b))
        end function
    end interface

    contains

    subroutine printWithSort (b, algor)
        class (base(4)), intent(in) :: b (:)

        procedure (sortAlg) :: algor

        integer tempIdx(size(b))

        tempIdx = algor(b)

        do i = 1, size(b)
            call b(tempIdx(i))%print
        end do
    end subroutine

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fArg025
use m
    procedure (sortAlg) :: reverseOrder, noSort

    type (child(4,1,20)) :: c1 (5), c2(10)
    class (base(4)), allocatable :: b1 (:)

    c1 = (/(child(4,1,20)(i, 'c1_'//char(ichar('0')+i)), i=1,5)/)

    c2 = (/(child(4,1,20) (i, 'c2_'//char(ichar('0')+i-1)), i=1,10)/)

    allocate (b1 (10), source=c2)

    call printwithSort (c1, reverseOrder)

    call printwithSort (b1(::2), noSort)

    call printWithSort ((/child(4,1,20)(100, 'temp1'), child(4,1,20)(200,'temp2')/), &
                        reverseOrder)
end

integer function reverseOrder (b)
use m
    class (base(4)), intent(in) :: b(:)
    dimension reverseOrder (size(b))

    reverseOrder = (/(i,i=size(b),1,-1)/)
end function

integer function noSort (b)
use m
    class (base(4)), intent(in) :: b(:)
    dimension noSort (size(b))

    noSort = (/(i,i=1,size(b))/)
end function
