! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2011-05-20
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : no module
!*                               if there is "=>" then shall have "::"
!*                               procedure ,nopass foo, bar => real_bar
!*                               with explicit attribute,1st is right,2nd is incorrect

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

        interface
          subroutine foo
          end subroutine

          subroutine real_bar
          end subroutine
        end interface

        type point
          contains
          procedure ,nopass foo, bar => real_bar  ! with explict attribute without :: then => will error
        end type point

      end program t