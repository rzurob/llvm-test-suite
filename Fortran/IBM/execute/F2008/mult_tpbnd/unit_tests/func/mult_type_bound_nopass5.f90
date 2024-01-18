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
!*                               more than 2 type bound procedures
!*                               procedure,nopass :: foo=>real_foo,bar,fox=>real_fox
!*                               this file is also reused in following scenario
!*                               mult_type_bound_langlvl2.scenario(-qlanglvl=2003pure)

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
      real function bar()
        bar = 1.0
        print *,"bar"
      end

      real function real_foo()
        real_foo = 1.0
        print *,"real_foo"
      end

      real function real_fox()
        real_fox = 1.0
        print *,"real_fox"
      end
      end module

      program t
        use m

        type point
          contains
          procedure,nopass :: foo=>real_foo,bar,fox=>real_fox
        end type point

        type(point) tp
        real rc
        rc = tp%foo()
        rc = tp%bar()
        rc = tp%fox()
      end program t
