! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Andy Chen
!*  DATE                       : September 14, 2010
!* .or.GIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : Test module procedures (function and
!                                subroutine).Argument type: array and scalar. 
!*
!*  KEY.or.(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HIS.or.
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
module mymod
contains
    integer function intreturn(x)
        integer, intent(in) ::x
        intreturn = x*2
    end function

    integer function arraysizereturn(x)
        integer, intent(in) ::x(:)
        arraysizereturn = size(x)
    end function

    subroutine arraysum(x, mysum)
        real, intent(in) ::x(:)
        real, intent(out) ::mysum
        do i = 1,10 
            mysum = mysum + x(i)
        end do
        return
    end subroutine
end module

program coindex_procedure_003f
    use mymod
    implicit none
    integer, save ::caf1[*], caf2(10)[*]
    real(4), save ::caf3(10)[*]
    real(4) sum1, sum2

    integer me, num, left, i, j
    interface
        logical function precision_r4 (r1, r2)
            real(4), intent(in) :: r1, r2
        end function
    end interface

    me = THIS_IMAGE()
    num = NUM_IMAGES()

    if (me == 1) then
        left = num
    else
        left = me - 1
    end if

    caf1 = me
    caf2 = [(i*me, i=1,10)]
    caf3 = [((i*me*1.0), i=1,10)]

    sync all
    
    if ( intreturn(caf1[left]) .ne. left*2) then
        error stop 1
    end if

    i = 5
    if ( intreturn(caf2(i)[left]) .ne. i*left*2) then
        error stop 2
    end if

    if (arraysizereturn(caf2(:)[me]) .ne. 10) then
        error stop 3
    end if

    sum1 = 0.0
    sum2 = 0.0

    do i=1,10
        sum2 = caf3(i) + sum2
        print *,'caf3=', caf3(i)[me]
        print *, sum2
    end do
    call arraysum(caf3(:)[me], sum1)

    if ( .not. precision_r4(sum1, sum2)) then
        print *, 'sum1=',sum1,' sum2=',sum2
        error stop 4
    end if 

end program

