!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : July 2011
!*  ORIGIN                     : AIX Compiler Development,
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
