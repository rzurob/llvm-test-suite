! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/point_assgn/fpAssgn009a5d.f
! opt variations: -qnol -qnodeferredlp

!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 04/27/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : data pointer assignment (diagnostic test case)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    interface makeData
        module procedure makeSeqData1
        module procedure makeSeqData2
    end interface

    contains

    class(*) function makeSeqData1 (i1)
        pointer makeSeqData1
        integer(8), intent(in) :: i1(:)

        type seq1(n1,k1)    ! (20,8)
            integer, kind :: k1
            integer, len  :: n1
            integer(k1)      i, j
        end type

        nullify (makeSeqData1)

        if (size(i1) == 2) then
            allocate (makeSeqData1, source=seq1(20,8)(i1(1), i1(2)))
        end if
    end function

    class(*) function makeSeqData2 (i1, isize)
        pointer makeSeqData2 (:)
        integer(8), intent(in) :: i1(:)
        integer, intent(in) :: isize

        type seq1(n2,k2)    ! (20,8)
            integer, kind :: k2
            integer, len  :: n2
            integer(k2)      i, j
        end type

        nullify (makeSeqData2)

        if (size(i1) == 2) then
            allocate (makeSeqData2(isize), source=seq1(20,8)(i1(1), i1(2)))
        end if
    end function
end module

program fpAssgn009a5d
use m
    type seq1(n3,k3)    ! (20,8)
        integer, kind :: k3
        integer, len  :: n3
        integer(k3)      i, j
    end type

    type(seq1(:,8)), pointer :: s1, s2(:)

    s1 => makeData((/1_8, 10_8/))           !<-- illegal

    s2 => makeData((/-1_8, -10_8/), 2)      !<-- illegal
end
