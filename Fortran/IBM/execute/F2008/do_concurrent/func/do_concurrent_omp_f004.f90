!*
!*  ===================================================================
!*
!*  TYPE                       : Functional test
!*  FEATURE                    : RTC 17293 - F2008: DO CONCURRENT Construct
!*
!*  DATE                       : August 12, 2015
!*
!*  REQUIRED COMPILER OPTIONS  : -qsmp
!*
!*  DESCRIPTION                : verifies correct processing with different Thread Wait Policy
!*                               settings through OMP_WAIT_POLICY environment variable with
!*                               DO CONCURRENT
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod
implicit none

contains
   subroutine sub3(i3_1, i3_2)
     integer :: j1,j2,j3,j4,j5,j6,j7,j8,j9
     integer :: k1,k2,k3,k4,k5,k6,k7,k8,k9
     integer, dimension (10:13,5:20,997:1000,(-10004):(-10002),-2:*) :: i3_1, i3_2
     ! Test 3
     do concurrent (j1 = 13:10:(-1))
       do concurrent (j2 = 20:5:(-2), j3 = 997:1000:1, j4 = (-10002):(-10004):(-2))
         do concurrent (j5 = 1:-2:(-2))
           i3_1(j1,j2,j3,j4,j5) = j1 + j2 + j3 + j4 +j5
           i3_2(j1,j2,j3,j4,j5) =&
             & i3_1(10-j1+13,5-j2+20,&
             &      1000-j3+997,(-10004)-j4+(-10002),-2-j5+1) + j2 + j4
         end do
       end do
     end do

   print *, "Test 3"
   write(*, "(8I10)" ) (((((i3_1 (j1,j2,j3,j4,j5) , j1=13,10,(-1)), &
                                            j2=20,5,(-2)),&
                                            j3=997,1000,1),&
                                            j4=(-10002),(-10004),(-2)),&
                                            j5=1,-2,(-2))

   write(*, "(8I10)" ) (((((i3_2 (j1,j2,j3,j4,j5) , j1=13,10,(-1)), &
                                            j2=20,5,(-2)),&
                                            j3=997,1000,1),&
                                            j4=(-10002),(-10004),(-2)),&
                                            j5=1,-2,(-2))
    end subroutine sub3
end module mod


program main
use mod
use omp_lib
implicit none
   integer :: j1,j2,j3,j4,j5,j6,j7,j8,j9
   integer :: k1,k2,k3,k4,k5,k6,k7,k8,k9

   integer :: th_limit, th_num
   integer, dimension(4,4) :: i1_2
   integer, dimension(4,4,4,4) :: i1_1

   integer, dimension (10:13,5:20,997:1000,(-10004):(-10002),-2:1) :: i3_1, i3_2
   integer, dimension (1:4,11:14,101:104,1001:1004) :: i5_1

   interface
      integer function my_sum ( arr)
        integer, dimension(:) :: arr
      end function my_sum
   end interface

   if (omp_get_nested()) then
      th_limit = omp_get_thread_limit()
      th_num=sqrt(real(th_limit))
      if ((th_num**2) > th_limit ) then
         print *," Incorrect thread num calc when omp nested - th_limit=",th_limit," th_num=",th_num
         error stop 17
      end if
      call omp_set_num_threads(th_num)
   end if

   ! Test 2 -- Explicit Array, Internal Subroutine
   call sub2

   ! Test 3 -- Assumed-size array, module subroutine
    i3_1=0.0
   call sub3(i3_1,i3_2)

   ! Test 5 -- Array, main
   i5_1 = 0
   !$OMP PARALLEL PRIVATE(j1,j2,j4,j5) REDUCTION(+:i5_1)
   do concurrent (j1 = 1:4:1, j2 = 11:14:1)
     do concurrent (j4 = 1001:1004:1, j5 = 10001:10004:1)
       i5_1(j1,j2,:,j4) = (/ (k1,k1=101,104) /) + i5_1(j1,j2,:,j4)
     end do
   end do
   !$OMP END PARALLEL

   print *, "Test 5"
   write(*, "(12I6)" ) ((((i5_1 (j1,j2,j3,j4), j1=1+1,4,1), &
                                               j2=11+1,14-1,1+1),&
                                               j3=101,104-1,1+1),&
                                               j4=1001+1,1004-1,1+1)
contains
   subroutine sub2
   implicit none
   integer :: j1,j2,j3,j4,j5,j6,j7,j8,j9
   integer :: k1,k2,k3,k4,k5,k6,k7,k8,k9

   integer, dimension ((-1):6,(-100):(-95),1001:1016) :: i2_1

   ! Test 2
   i2_1 = 0
   !$OMP PARALLEL PRIVATE(j1,j2,j3)
   do concurrent (j1 = (-1):6:1, j2 = (-100):(-95):2)
     do concurrent (j3 = 1001:1016:2)

       !$OMP PARALLEL DO PRIVATE(j4,j5) REDUCTION(+:i2_1)
       do j4 = 10000, 10004, 1
         do j5 = 1, 8, 2
           i2_1(j1,j2,j3) = j3+j4+j5 + i2_1(j1,j2,j3)
         end do
       end do
       !$OMP END PARALLEL DO

     end do
   end do
   !$OMP END PARALLEL

   print *, "Test 2"
   write(*, "(6I12)" ) (((i2_1(j1,j2,j3), j1=(-1)+1,6,1), j2=(-100),(-95),2+1), j3=1001,1016,2)

   end subroutine sub2
end program
