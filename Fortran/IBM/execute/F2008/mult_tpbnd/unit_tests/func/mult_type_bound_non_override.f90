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
!*                               more than 2 type bound procedures
!*                               non_overridable attribute
!*                               procedure, non_overridable, nopass :: foo => real_foo, &
!*                                        fox => real_fox,bar => real_bar, cat => real_cat

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
          procedure, non_overridable, nopass :: foo => real_foo,fox => real_fox,bar => real_bar, cat => real_cat
        end type

        contains
        subroutine real_foo()
          print *, "real_foo"
        end subroutine

        subroutine real_fox()
          print *, "real_fox"
        end subroutine

        subroutine real_cat()
          print *, "real_cat"
        end subroutine

        subroutine real_bar()
          print *, "real_bar"
        end subroutine

      end module

      program t
        use m
        type(point) :: tp1
        call tp1%foo()
        call tp1%fox()
        call tp1%cat()
        call tp1%bar()
      end program t
