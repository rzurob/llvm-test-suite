! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/point_assgn/fpAssgn027.f
! opt variations: -qnol -qnodeferredlp

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn027.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/27/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (unlimited function
!                               return results in the RHS)
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
    type seq(n1,k1,k2)    ! (20,4,8)
        integer, kind :: k1,k2
        integer, len  :: n1
        sequence
        integer(k1)      i1
        integer(k2)      i2
    end type

    type, bind(C) :: bT
        real*4 :: data
    end type
end module

program fpAssgn027
use m
    interface createData
        function createInt (i)
            class (*), pointer :: createInt
            integer*4, intent(in) :: i
        end function

        function createSeq (i1, i2)
            class(*), pointer :: createSeq
            integer*4, intent(in) :: i1
            integer*8, intent(in) :: i2
        end function

        function createBT (c)
            class(*), pointer :: createBT
            real*4, intent(in) :: c
        end function
    end interface

    logical precision_r4
    type (seq(:,4,8)), pointer :: s1
    type (bT), pointer :: b1

    class (*), pointer :: x

    s1 => createData (10_4, 20_8)

    b1 => createData (1.0)

    x => createData (100_4)

    if ((s1%i1 /= 10) .or. (s1%i2 /= 20_8)) error stop 1_4

    if (.not. precision_r4 (b1%data, 1.0000)) error stop 2_4

    deallocate (s1, b1, x)
end

function createInt (i)
    class (*), pointer :: createInt
    integer*4, intent (in) :: i

    integer*4, pointer :: i_tmp

    allocate (i_tmp)
    i_tmp = i

    createInt => i_tmp
end function


function createSeq (i1, i2)
    type seq(n2,k3,k4)    ! (20,4,8)
        integer, kind :: k3,k4
        integer, len  :: n2
        sequence
        integer(k3)      i1
        integer(k4)      i2
    end type

    class(*), pointer :: createSeq
    integer*4, intent(in) :: i1
    integer*8, intent(in) :: i2

    type(seq(:,4,8)), pointer :: temp

    allocate (temp, source=seq(20,4,8)(i1, i2))

    createSeq => temp
end function


function createBT (c)
use m
    class(*), pointer :: createBT
    real*4, intent(in) :: c

    type (bT), pointer :: temp

    allocate (temp, source=bT(c))

    createBT => temp
end function
