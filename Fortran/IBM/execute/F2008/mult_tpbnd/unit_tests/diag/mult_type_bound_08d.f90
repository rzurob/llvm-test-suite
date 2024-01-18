! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : mult_type_bound_diag8.f 
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
!*  DESCRIPTION                : within module
!*                               private type bound procedure can't be accessed outof module
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
          call a%dog()
          print *, "real_bar"
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
        call tp1%dog()
        call tp1%cat()
      end program t
