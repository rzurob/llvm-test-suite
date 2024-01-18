!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : Testing  large character strings
!*                               with deferred length
!*
!*  ===================================================================

      integer, parameter :: l = 267465346
      character (:), allocatable :: ch
      allocate (character(l+10) :: ch)
      ch(l:l+10)= "12345678901234567890"
      if (ch(l:l+10) .ne. "12345678901") error stop 1
      deallocate(ch)
      end
