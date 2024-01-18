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
!*                               if there is "=>" then shall have "::"
!*                               procedure foo, bar => real_bar
!*                               with implicit attribute,1st is right,2nd is incorrect

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
        type point(K)
          integer, kind :: K
          integer(K):: data_a
          contains
          procedure foo, bar => real_bar  ! with implicit attribute without :: then => will error
        end type point

        contains
        subroutine foo(a)
          class(point(4)) :: a
        end subroutine

        subroutine real_bar(a)
          class(point(4)) :: a
        end subroutine
      end module

      program t

      end program t