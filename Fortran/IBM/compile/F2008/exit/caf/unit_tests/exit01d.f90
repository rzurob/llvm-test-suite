!* =================================================================== &
!*
!* DATE                       : March 8, 2011
!* ORIGIN                     : AIX Compiler Development,
!*
!* PRIMARY FUNCTIONS TESTED   : EXIT Statement
!*
!* DESCRIPTION                : Testing proper diagnostics of
!*                              EXIT statement in critical
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program exit01d

      critical
        exit
      end critical

      crit : critical
        exit
      end critical crit

      crit2 : critical
        exit crit2
      end critical crit2

      crit3 : critical
        exit redherring
      end critical crit3

      end
