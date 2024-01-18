! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2011-05-20
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : within module
!*                               nopass attribute
!*                               procedure,nopass :: foo=>real_foo,bar=>real_bar

!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      module m
      contains
      real function real_foo()
	    real_foo = 1.0
        print *,"real_foo"
      end

      real function real_bar()
	    real_bar = 1.0
        print *,"real_bar"
      end
      end module

      program t
        use m

        type point
          contains
          procedure,nopass :: foo=>real_foo,bar=>real_bar
        end type point

        type(point) tp
        real rc
        rc = tp%foo()
        rc = tp%bar()
      end program t
