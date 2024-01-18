! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Andy Chen
!*  DATE                       : September 23, 2010
!* .or.GIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : Test internal procedures (function and
!                                subroutine).Argument type: array and scalar.
!                                corank = 4 
!*
!*  KEY.or.(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HIS.or.
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

program coindex_procedure_006f
    implicit none
    integer, save ::caf1[1,4,1,*], caf2(10)[2,2,2,*]
    real(4), save ::caf3(10)[2,2,2,*]
    real(4) sum1, sum2

    integer me, num, left, i, j, k, l, m, n
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
    
    do i = 1, 4 
        do j = 1, 2
            l = image_index(caf1, [1,i,1,j])
            if ( intreturn(caf1[1,i,1,j]) .ne. l*2) then
                error stop 1
            end if
        end do
    end do

    do i = 1, 2 
        do j = 1, 2
            do k = 1,2
                do l = 1,2
                    n = image_index(caf2, [i,j,k,l]) 
                    do m = 1, 10
                        if ( intreturn(caf2(m)[i,j,k,l]) .ne. m*n*2) then
                            error stop 2
                        end if
                    end do
                end do
            end do
        end do
    end do
    

    do i = 1, 2 
        do j = 1, 2
            do k = 1,2
                do l = 1,2
                    if (arraysizereturn(caf2(:)[i,j,k,l]) .ne. 10) then
                        error stop 3
                    end if
                end do
            end do
        end do
    end do

    do i = 1, 2 
        do j = 1, 2
            do k = 1,2
                do l = 1,2
                    n = image_index(caf2, [i,j,k,l]) 
                    sum1 = 0.0
                    sum2 = 0.0
                    do m=1,10
                        sum2 = caf3(m)[i,j,k,l] + sum2
                    end do
                    call arraysum(caf3(:)[i,j,k,l], sum1)
                    if ( .not. precision_r4(sum1, sum2)) then
                        print *, 'sum1=',sum1,' sum2=',sum2
                        error stop 4
                    end if 
                end do
            end do
        end do
    end do

    contains
    integer function intreturn(x)
        integer, intent(in) ::x
        intreturn = x * 2
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

end program
