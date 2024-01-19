! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-09-14
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - Combination of attributes CONTIGUOUS,
!*                                  PROTECTED
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module myMod

    real, contiguous, pointer, protected :: ptr(:)
    real, target :: arr1(10), arr2(10)
    logical, external :: precision_r4

contains
    subroutine sub1(arg)
        real, target :: arg(10)
        ptr => arg
    end subroutine

    subroutine update(arg)
        real :: arg(10)
        arg = arg + 1
    end subroutine

    subroutine printf()
        do i = 1, 10
          if (.not. precision_r4(ptr(i), i-10.0)) ERROR STOP 11
        end do
    end subroutine

end module

program main

    use myMod

    arr1 = (/(i,i=-10,-1)/)

    call sub1(arr1)
    call update(arr1)
    call printf()
end
