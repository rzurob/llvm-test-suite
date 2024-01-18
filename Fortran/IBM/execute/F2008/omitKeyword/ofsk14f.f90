!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : ofsk14f.f
!*
!*  PROGRAMMER                 : Jin Li
!*  DATE                       : 2010-09-30
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 Omit FUNCTION and SUBROUTINE Keywords
!*  REFERENCE                  : Feature Number 376084
!*  REQUIRED COMPILER OPTIONS  : noopt
!*
!*  DESCRIPTION
!*
!*  Nested internal function within module's function is terminated by "END",
!*  and the module procedure (function) is terminated by "END" too.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789
	module m
	contains
	  function func1() 
        func1 =func2()
	    contains
	      function func2() 
	        func2 = 2
	      end 
	  end  
	end

    program main
    use m
    a=func1()
    print *, a
    end
