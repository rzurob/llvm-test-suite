! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Andy Chen
!*  DATE                       : September 27, 2010
!* .or.GIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : Test external procedures (function and subroutine)
!                                Argument type: array and scalar. corank = 3
!*
!*  KEY.or.(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HIS.or.
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

program coindex_procedure_007f
    implicit none
    integer, save ::caf1[1,2,*], caf2(10)[3,1,*]
    real(4), save ::caf3(10)[1,3,*]
    real(4) sum1, sum2
    integer me, num, left, i,j, k, m
    interface
        logical function precision_r4 (r1, r2)
            real(4), intent(in) :: r1, r2
        end function
        integer function intreturn(x)
            integer, intent(in) ::x
        end function
        integer function arraysizereturn(x)
            integer, intent(in) ::x(:)
        end function
        subroutine arraysum(x, mysum)
            real, intent(in) ::x(:)
            real, intent(out) ::mysum
        end subroutine
    end interface

    me = THIS_IMAGE()
    num = NUM_IMAGES()

    if (me == 1) then
        left = num
    else
        left = me - 1
    end if

    caf1 = me
    caf2 = [(k*me, k=1,10)]
    caf3 = [((k*me*1.0), k=1,10)]

    sync all
    
    do i=1,2
        do j=1,3
            m = image_index(caf1, [1,i,j])
            if ( intreturn(caf1[1,i,j]) .ne. m*2 ) then
                error stop 1
            end if
        end do
    end do

    do i=1,3
        do j=1,2
            m = image_index(caf2, [i,1,j])
            do k = 1, 10
                if ( intreturn(caf2(k)[i,1,j]) .ne. k*m*2 ) then
                    error stop 2
                end if
            end do
        end do
    end do

    do i=1,3
        do j=1,2
            if (arraysizereturn(caf2(:)[i,1,j]) .ne. 10 ) then
                error stop 3
            end if
        end do
    end do

    do i=1,3
        do j=1,2
            sum1 = 0.0
            sum2 = 0.0
            do k=1,10
                sum2 = caf3(k)[1,i,j] + sum2
            end do
            call arraysum(caf3(:)[1,i,j], sum1)
            if ( .not. precision_r4(sum1, sum2)) then
                print *, 'sum1=',sum1,' sum2=',sum2
                error stop 4
            end if 
        end do
    end do

end program

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
