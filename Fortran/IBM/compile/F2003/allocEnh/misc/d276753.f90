!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/05/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 276753)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, private :: seq1
        sequence
        integer*4 :: x
    end type

    type (seq1) :: s1, s2


    contains

    subroutine assgnS1toS2
        s2 = s1
    end subroutine
end module

use m, only : s1, s2, assgnS1toS2
      type seq1
        sequence
        integer*4 :: x
      end type
    type (seq1) :: s11

    call assgnS1toS2

    s11 = s2    !<-- this should not work
end


