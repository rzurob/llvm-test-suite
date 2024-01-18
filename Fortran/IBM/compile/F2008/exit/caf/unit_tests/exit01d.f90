!* =================================================================== &
!* XL Fortran Test Case                          IBM INTERNAL USE ONLY
!* =================================================================== &
!*
!* TEST CASE TITLE            : exit01d.f
!*
!* PROGRAMMER                 : David Nichols
!* DATE                       : March 8, 2011
!* ORIGIN                     : AIX Compiler Development,
!*                            : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : EXIT Statement
!*
!* DRIVER STANZA              : xlf2008
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
