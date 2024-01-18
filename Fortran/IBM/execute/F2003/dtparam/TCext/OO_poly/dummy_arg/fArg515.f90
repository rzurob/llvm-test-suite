! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg515.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg515.f
! %VERIFY: fArg515.out:fArg515.vf
! %STDIN:
! %STDOUT: fArg515.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/17/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (dummy-procedure as the
!                               argument in the binding call)
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
        integer(k1)   :: id (5)

        contains

        procedure :: print => printWithSort
    end type

    contains

    subroutine printWithSort (b, sort)
        class (base(4)), intent(in) :: b

        interface
            subroutine sort (i1, out1)
                integer*4, intent(in) :: i1(:)
                integer*4, intent(out) :: out1(:)
            end subroutine
        end interface

        integer*4 :: temp (size(b%id))

        call sort (b%id, temp)

        do i = 1, size (temp)
            print *, temp (i)
        end do
    end subroutine
end module

module m1
    contains

    !! this subroutine does a direct assignment
    subroutine noReorder (i1, out1)
        integer*4, intent(in) :: i1(:)
        integer*4, intent(out) :: out1(:)

        out1 = i1
    end subroutine

    !! this routine does a bubble sort
    subroutine bubbleSort (i1, out1)
        integer*4, intent(in) :: i1(:)
        integer*4, intent(out) :: out1(:)

        integer*4 temp

        out1 = i1
        do i = size (out1), 1, -1
            do j = 2, i
                if (out1(j-1) > out1 (j)) then
                    temp = out1(j-1)
                    out1(j-1) = out1 (j)
                    out1 (j) = temp
                end if
            end do
        end do
    end subroutine
end module

program fArg515
use m
use m1
    type (base(4)) :: b1

    b1%id = (/3, 1, 6, 2, 9/)

    print *, 'no sorting'

    call b1%print (noReorder)


    print *, 'applying bubble sort'

    call b1%print (bubbleSort)
end
