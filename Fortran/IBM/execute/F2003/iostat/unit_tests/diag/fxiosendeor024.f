!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 26, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end and is_iostat_eor intrinsics
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qdebug=intmsg
!*
!*  DESCRIPTION                : This diagnostic test, tests the error messages produced
!*                               when an incorrect dummy argument name is used.
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      logical ignore

      ignore = is_iostat_end(i=-1)
      ignore = is_iostat_end(j=-1)
      ignore = is_iostat_end(h=-1)
      ignore = is_iostat_end(k=-1)
      ignore = is_iostat_end(l=-1)

      ignore = is_iostat_eor(i=-4)
      ignore = is_iostat_eor(j=-4)
      ignore = is_iostat_eor(h=-4)
      ignore = is_iostat_eor(k=-4)
      ignore = is_iostat_eor(l=-4)


      end
