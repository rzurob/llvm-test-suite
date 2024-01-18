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
!*
!*  DATE                       : 06/03/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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
    type base
        integer*4 :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    interface
        integer function sortAlg (b)
        import base
            class (base), intent(in) :: b(:)
            dimension sortAlg(size(b))
        end function
    end interface

    contains

    subroutine printWithSort (b, algor)
        class (base), intent(in) :: b (:)

        procedure (sortAlg) :: algor

        integer tempIdx(size(b))

        tempIdx = algor(b)

        do i = 1, size(b)
            call b(tempIdx(i))%print
        end do
    end subroutine

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fArg025
use m
    procedure (sortAlg) :: reverseOrder, noSort

    type (child) :: c1 (5), c2(10)
    class (base), allocatable :: b1 (:)

    c1 = (/(child(i, 'c1_'//char(ichar('0')+i)), i=1,5)/)

    c2 = (/(child (i, 'c2_'//char(ichar('0')+i-1)), i=1,10)/)

    allocate (b1 (10), source=c2)

    call printwithSort (c1, reverseOrder)

    call printwithSort (b1(::2), noSort)

    call printWithSort ((/child(100, 'temp1'), child(200,'temp2')/), &
                        reverseOrder)
end

integer function reverseOrder (b)
use m
    class (base), intent(in) :: b(:)
    dimension reverseOrder (size(b))

    reverseOrder = (/(i,i=size(b),1,-1)/)
end function

integer function noSort (b)
use m
    class (base), intent(in) :: b(:)
    dimension noSort (size(b))

    noSort = (/(i,i=1,size(b))/)
end function
