!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/09/2007
!*
!*  DESCRIPTION                : miscellaneous (defect 312513)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
use iso_c_binding
    integer(c_int), bind(c, name='x1') :: i1


    interface
        subroutine test () bind(c, name='testC')
        end subroutine
    end interface
end module

module m1
use iso_c_binding
use m

    real (C_DOUBLE), bind(c, name='X1') :: r1
    interface
        subroutine test1 () bind(c, name='testc')
        end subroutine
    end interface
end module

program d312513
use m1
    i1 = 100
    r1 = 3.14159265358d0

    call test

    call test1

    write (*, '(i4, 1x, f15.13)') i1, r1
end

