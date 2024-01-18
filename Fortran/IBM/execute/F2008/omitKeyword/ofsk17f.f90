!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : ofsk17f.f
!*
!*  DATE                       : 2010-09-30
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 Omit FUNCTION and SUBROUTINE Keywords
!*  REFERENCE                  : Feature Number 376084
!*  REQUIRED COMPILER OPTIONS  : noopt
!*
!*  DESCRIPTION
!*
!*  Nested internal function within module's subroutine is terminated by "END",
!*  and the module procedure (subroutine) is terminated by "END SUBROUTINE"
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789
	module m
	  contains
	    subroutine sub1()
          integer i
          i = func2()
          print *, "in sub1 "
          print *, i
	      contains
	      function func2()
	        func2 = 2
	      end
	    end subroutine
	end

    program main
    use m
    call sub1
    end
