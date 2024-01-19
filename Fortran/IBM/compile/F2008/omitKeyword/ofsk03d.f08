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
!*  Error message expected when it has three-level nested internal procedures.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789
	module m
	contains
	  function func1()
        func1 = func2()
	    contains
	      function func2()
	        func2 = func3()
            contains
              function func3()
              func3 = 3
              end
	      end
	   end
	end

    program main
    use m
    a=func1()
    print *, a
    end
