!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : d380624.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : July 2011
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Common blocks should not contain coarrays
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
        integer, save :: caf2[11:*]
        common /data/ caf2

        call sub2()
end


subroutine sub2()
        integer, save :: a[1,2,3:*]
        common /data/ a
end subroutine
