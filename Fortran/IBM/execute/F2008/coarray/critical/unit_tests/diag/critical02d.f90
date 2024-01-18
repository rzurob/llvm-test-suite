!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : critical02d.f
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

      module m

      ! Executable in specification: illegal
      critical 
      end critical

      contains

      subroutine critical_in_sub()
        named_mod : critical
	  critical 
          end critical
        end critical named_mod
      end subroutine critical_in_sub

      subroutine calling_sub_w_crit()
        critical
          call critical_in_sub()
        end critical
        abc : critical
          call critical_in_sub()
        end critical abc
      end subroutine calling_sub_w_crit

      end module m

      program critical02d

      ! Ending non-existent critical block
      end critical
      end critical ghost

      ! Missing end critical statement
      ghost : critical

      contains

      subroutine named_crit ()
        ghost : critical 
      end subroutine named_crit

      subroutine named_end ()
        end critical ghost
        end critical
      end subroutine named_end

      end
