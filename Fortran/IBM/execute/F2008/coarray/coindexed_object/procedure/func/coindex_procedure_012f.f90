! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : October 5, 2010
!* .or.GIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test external procedures (function and
!                                subroutine).Argument type: array and scalar.
!                                assume-size, explicit-size
!*
!*  KEY.or.(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HIS.or.
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

program coindex_procedure_012f
    implicit none
    real(4), save ::caf3(10)[*]
    real(4) sum1, sum2, sum3
    integer me, i
    interface
        logical function precision_r4 (r1, r2)
            real(4), intent(in) :: r1, r2
        end function
        subroutine arraysum1(x,n, mysum)
            integer, intent(in) ::n
            real, intent(in) ::x(n)
            real, intent(out) ::mysum
        end subroutine
        real(4) function arraysum2(x, n)
            integer, intent(in) ::n
            real, intent(in) ::x(*)
        end function
    end interface

    me = THIS_IMAGE()

    caf3 = [((i*me*1.0), i=1,10)]

    sync all

    sum1 = 0.0
    sum2 = 0.0

    call arraysum1(caf3(:)[me], 10, sum1)
    do i=1,10
        sum2 = caf3(i)[me] + sum2
    end do
    sum3 = arraysum2(caf3(:)[me], 10)

    if ( .not. precision_r4(sum1*2.0, sum2+sum3)) then
        print *, 'sum1=',sum1,' sum2=',sum2,' sum3=', sum3
        error stop 1
    end if

end program

subroutine arraysum1(x,n, mysum)
    integer, intent(in) ::n
    real, intent(in) ::x(n)
    real, intent(out) ::mysum
    do i = 1,size(x)
        mysum = mysum + x(i)
    end do
    return
end subroutine

real(4) function arraysum2(x, n)
    integer, intent(in) ::n
    real(4), intent(in) ::x(*)
    real(4) ::y
    y = 0.0
    do i = 1, n
        y = y + x(i)
    end do
    arraysum2 = y
end function
