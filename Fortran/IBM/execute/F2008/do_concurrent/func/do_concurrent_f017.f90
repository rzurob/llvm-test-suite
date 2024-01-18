!*******************************************************************************
!*
!============================================================================
!*
!============================================================================
!*
!*  TEST CASE NAME             : F2008/do_concurrent/func/do_concurrent_f017.f
!*
!*  DATE                       : 2015-07-03
!*
!*  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT (F2008 extension)
!*
!*  DESCRIPTION                : - DO CONCURRENT loops (including nested DO
!*                                 CONCURRENT loops) inside subroutines and
!*                                 functions inside modules from external
!*                                 files
!*                               - concurrent-limit contains a variable with
!*                                 the parameter attribute
!*                               - scalar-mask-expr contains logicals and
!*                                 user-defined procedures
!*
!=============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890
      program main
        use m1; use m2

        implicit none

        logical, external :: precision_r4, precision_r8

        integer*1 :: i = 100
        integer :: x, y
        integer*2 :: j = 200
        integer*8 :: k = 400
        real*4, allocatable :: i_arr(:)
        real*8, allocatable :: i_arr2(:)
        real*8 :: i_res(5,5)
        real*8 :: i_res_result(5,5)
        double precision, allocatable :: i_res2(:), i_res3(:,:)
        double precision :: i_res2_result(10), i_res3_result(5,5)

        allocate(i_arr(5), i_arr2(10), i_res2(10), i_res3(5,5))
        i_arr = (/11.5e0, 22.5e0, 33.5e0, 44.5e0, 55.5e0/)
        i_arr2 = 10.5d0
        i_res = 0.0d0
        i_res2 = 0.0d0
        i_res3 = 0.0d0

        call sub5(i,j)
        i_res = func5(i,j)

        i_res_result(1,:) =  0.0d0
        i_res_result(2,:) = (/1.0d0, 2.0d0, 0.0d0, 4.0d0, 0.0d0/)
        i_res_result(3,:) = 0.0d0
        i_res_result(4,:) = (/1.0d0, 2.0d0, 0.0d0, 4.0d0, 0.0d0/)
        i_res_result(5,:) = (/1.0d0, 2.0d0, 0.0d0, 4.0d0, 0.0d0/)
        do x = 1,5
          do y = 1,5
            if ( .not. precision_r8(i_res(x,y),i_res_result(x,y)) ) then
              print *, "do concurrent with index in mask produced incorrect results"
              print *, "x: ", x
              print *, "y: ", y
              print *, "i_res: ", i_res
              error stop 1
            end if
          end do
        end do

        i_res = 0.0d0
        call sub4()
        i_res = func4()

        i_res_result(1,:) = 10.5d0
        i_res_result(2,:) = 21.0d0
        i_res_result(3,:) = 31.5d0
        i_res_result(4,:) = 42.0d0
        i_res_result(5,:) = 52.5d0
        do x = 1,5
          do y = 1,5
            if ( .not. precision_r8(i_res(x,y),i_res_result(x,y)) ) then
              print *, "2-level nested do concurrent with multiple indices produced incorrect results"
              print *, "x: ", x
              print *, "y: ", y
              print *, "i_res: ", i_res
              error stop 2
            end if
          end do
        end do

        i_res2 = 0.0d0
        i_arr2 = (/10.5d0,20.5d0,30.5d0,40.5d0,50.5d0,11.5d0,22.5d0,33.5d0,44.5d0,55.5d0/)
        call sub3(i_arr2)
        i_res2 = func3(i_arr2)

        i_res2_result = (/0.0d0,0.3d1,0.0d0,0.5d1,0.6d1,0.0d0,0.0d0,0.9d1,0.0d0,1.1d1/)
        do x = 1,10
          if ( .not. precision_r8(i_res2(x),i_res2_result(x)) ) then
            print *, "2-level nested do concurrent with mask produced incorrect results"
            print *, "x: ", x
            print *, "i_res2: ", i_res2
            error stop 3
          end if
        end do

        i_res3 = 0.0d0
        call sub2(j,k,i_arr)
        i_res3 = func2(j,k,i_arr)

        i_res3_result(1,:) = 0.0d0
        i_res3_result(2,:) = 0.0d0
        i_res3_result(3,:) = (/4.0d0,4.0d0,4.0d0,0.0d0,0.0d0/)
        i_res3_result(4,:) = (/5.0d0,5.0d0,5.0d0,0.0d0,0.0d0/)
        i_res3_result(5,:) = (/6.0d0,6.0d0,6.0d0,0.0d0,0.0d0/)
        do x = 1,5
          do y = 1,5
            if ( .not. precision_r8(i_res3(x,y),i_res3_result(x,y)) ) then
              print *, "2-level nested do concurrent with multiple indices produced incorrect results"
              print *, "x: ", x
              print *, "y: ", y
              print *, "i_res3: ", i_res3
              error stop 4
            end if
          end do
        end do

        i_res = 0.0d0
        i_arr2 = (/10.0d0,20.0d0,30.0d0,40.0d0,50.0d0,60.0d0,70.0d0,80.0d0,90.0d0,100.0d0/)
        call sub1(i,j,k,i_arr2)
        i_res = func1(i,j,k,i_arr2)

        i_res_result(1,:) = (/0.5d0, 0.5d0, 1.5d0, 0.5d0, 0.5d0/)
        i_res_result(2,:) = (/0.5d0, 0.5d0, 4.5d0, 4.5d0, 0.5d0/)
        i_res_result(3,:) = (/1.5d0, 4.5d0, 1.5d0, 4.5d0, 10.5d0/)
        i_res_result(4,:) = (/0.5d0, 4.5d0, 4.5d0, 4.5d0, 0.5d0/)
        i_res_result(5,:) = (/0.5d0, 0.5d0, 10.5d0, 0.5d0, 0.5d0/)
        do x = 1,5
          do y = 1,5
            if ( .not. precision_r8(i_res(x,y),i_res_result(x,y)) ) then
              print *, "3-level nested do concurrent with multiple indices and masks produced incorrect results"
              print *, "failure in third, inner-most loop"
              print *, "x: ", x
              print *, "y: ", y
              print *, "i_res: ", i_res
              error stop 5
            end if
          end do
        end do

      end program
