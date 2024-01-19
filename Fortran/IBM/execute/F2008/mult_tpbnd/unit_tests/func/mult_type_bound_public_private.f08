! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2011-05-20
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : within module
!*                               public & private
!*                               procedure, public, nopass :: foo => real_foo,fox => real_fox
!*                               procedure, private,nopass :: dog => real_dog, cat => real_cat

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
        type point
          contains
          procedure, public, nopass :: foo => real_foo,fox => real_fox
          procedure, private,nopass :: dog => real_dog, cat => real_cat
        end type

        contains
        subroutine real_foo()
          print *, "real_foo"
        end subroutine

        subroutine real_fox()
          print *, "real_fox"
        end subroutine

        subroutine real_bar(a)
          type(point) :: a
          print *, "call private subroutine start"
          call a%dog()
          call a%cat()
          print *, "call private subroutine end"
        end subroutine

        subroutine real_cat()
          print *, "real_cat"
        end subroutine

        subroutine real_dog()
          print *, "real_dog"
        end subroutine

      end module

      program t
        use m
        type(point) :: tp1
        call tp1%foo()
        call tp1%fox()
        call real_bar(tp1)
      end program t
