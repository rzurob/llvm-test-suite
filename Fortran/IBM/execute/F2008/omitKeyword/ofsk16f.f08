!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-09-30
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 Omit FUNCTION and SUBROUTINE Keywords
!*  REFERENCE                  : Feature Number 376084
!*  REQUIRED COMPILER OPTIONS  : noopt
!*
!*  DESCRIPTION
!*
!*  Nested internal subroutine within module's function is terminated by
!*  "END", and the module procedure (function) is terminated by "END"
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789
	module m
	contains
	  function func1()
        call sub2
        func1 = 1
	    contains
          subroutine sub2()
            print *, " in sub2 "
	      end
	   end
	end

    program main
    use m
    a=func1()
    print *, a
    end
