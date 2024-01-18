! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : September 14, 2010
!* .or.GIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test module procedures (function and
!                                subroutine).Argument type: array and scalar.
!                                corank = 2
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

program coindex_procedure_008f
    use mymod
    implicit none
    integer, save ::caf1[3,*], caf2(10)[4,*]
    real(4), save ::caf3(10)[2,*]
    real(4) sum1, sum2

    integer me, num, left, i, j, m, k
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
    do i=1,3
        do j=1,4
            m = image_index(caf1, [i,j])
            if ( intreturn(caf1[i,j]) .ne. m*2) then
                error stop 1
            end if
        end do
    end do

    do i=1,4
        do j=1,3
            m = image_index(caf2, [i,j])
            if ( intreturn(caf2(5)[i,j]) .ne. 5*m*2) then
                error stop 2
            end if
        end do
    end do

    do i=1,4
        do j=1,3
            if (arraysizereturn(caf2(:)[i,j]) .ne. 10) then
                error stop 3
            end if
        end do
    end do

    do i=1,2
        do j=1,6
            sum1 = 0.0
            sum2 = 0.0
            do k=1,10
                sum2 = caf3(k)[i,j] + sum2
            end do
            call arraysum(caf3(:)[i,j], sum1)
            if ( .not. precision_r4(sum1, sum2)) then
                print *, 'sum1=',sum1,' sum2=',sum2
                error stop 4
            end if
        end do
    end do

end program

