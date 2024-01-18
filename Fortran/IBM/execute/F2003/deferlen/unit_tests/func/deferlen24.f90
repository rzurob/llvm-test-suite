!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : Testing the allocatable attributes on
!*                               characters with deferred length.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
      character(:), allocatable :: aa(:)
      allocate (character(10)::aa(5))
      aa = 'A'

      if (len(aa(1)) .ne. 1) error stop 1
      if (aa(1) /= 'A')   error stop 2

      deallocate(aa)
      end

