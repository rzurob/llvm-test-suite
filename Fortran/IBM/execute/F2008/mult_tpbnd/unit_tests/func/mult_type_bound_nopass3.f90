! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : mult_type_bound_nopass3.f 
!*
!*  PROGRAMMER                 : Paul Liu
!*  DATE                       : 2011-05-20
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : no module
!*                               nopass attribute
!*                               procedure,nopass :: foo,bar=>real_bar

!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      real function foo()
	    foo = 1.0
        print *,"foo"
      end

      real function real_bar()
	    real_bar = 1.0
        print *,"real_bar"
      end


      program t

        interface
          real function foo()
          end function
          real function real_bar()
          end function
        end interface
  
        type point
          contains
          procedure,nopass :: foo,bar=>real_bar
        end type point

        type(point) tp
        real rc
        rc = tp%foo()
        rc = tp%bar()
      end program t
