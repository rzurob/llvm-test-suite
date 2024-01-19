! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2011-05-20
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : no module
!*                               redefinition
!*                               procedure,nopass :: foo, foo => real_foo2 ! 2 foo

!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      program t

        type point
          contains
          procedure,nopass :: foo, foo => real_foo2 ! 2 foo
        end type point

      end program t