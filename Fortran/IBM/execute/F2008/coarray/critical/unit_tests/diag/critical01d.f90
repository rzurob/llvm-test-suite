!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : critical01d.f
!*
!*  PROGRAMMER                 : David Nichols
!*  DATE                       : Oct 13, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CRITICAL Construct
!*
!*  DRIVER STANZA              : xlf2008
!*
!*  DESCRIPTION                : Testing proper diagnostics of
!*                               the F2008 CRITICAL Construct
!*                               Testing: 8.1.5 CRITICAL construct
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program critical01d

      ! 8.1.5: A CRITICAL construct limits execution of a block to one 
      !        image at a time.

      ! R810 critical-construct   is   critical-stmt
      !                                  block
      !                                end-critical-stmt

      ! R811 critical-stmt   is   [ critical-construct-name : ] CRITICAL
      ! R812 end-critical-stmt   is   END CRITICAL [ critical-construct-name ]

      critical
      end critical

      crit : critical
      end critical crit

      ! C809 (R810) If the critical-stmt of a critical-construct specifies a
      !             critical-construct-name, the corresponding end-critical-stmt
      !             shall specify the same critical-construct-name. If the 
      !             critical-stmt of a critical-construct does not specify a 
      !             critical-construct-name, the corresponding end-critical-stmt 
      !             shall not specify a critical-construct-name.

      missing_end : critical
      end critical

      critical
      end critical missing_name

      match : critical 
      end critical mismatch

      end
