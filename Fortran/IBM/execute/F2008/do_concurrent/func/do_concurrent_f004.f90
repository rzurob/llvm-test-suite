!*******************************************************************************
!*
!============================================================================
!*  XL Fortran Test Case                              IBM INTERNAL USE ONLY
!*
!============================================================================
!*
!*  TEST CASE NAME             : F2008/do_concurrent/func/do_concurrent_f004.f
!*
!*  PROGRAMMER                 : Nicole Negherbon
!*  DATE                       : 2015-03-26
!*
!*  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT (F2008 extension)
!*
!*  DESCRIPTION                : - Various kinds of integers, reals and double 
!*                                 allocatable arrays in DO CONCURRENT loops 
!*                                 including nested DO CONCURRENT loops
!*                               - concurrent-limit contains a variable with 
!*                                 the parameter attribute
!*                               - scalar-mask-expr contains logicals and 
!*                                 allocatable arrays of various real kinds
!*                               - Use of functions inside DO CONCURRENT loops
!*                               - Use of functions in the scalar-mask-expr
!*
!=============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890
      program main
        implicit none

        logical, external :: precision_r4, precision_r8

        integer*1 :: i = 100, x, y
        integer*2 :: j = 200
        integer*8 :: k = 300
        integer :: l = 400
        integer, parameter :: m = 5
        real*4, allocatable :: i_res(:), i_arr(:), i_arr2(:)
        real*8, allocatable :: i_arr3(:), i_res2(:,:)
        real*8 :: i_res2_result(5,5)
        double precision, allocatable :: i_res3(:), i_res4(:), i_res5(:,:)
        double precision :: i_res3_result(10), i_res4_result(10), i_res5_result(5,5)
        double precision, allocatable :: i_arr4(:)
        logical :: lvar = .true.

        allocate(i_res(5), i_arr(5), i_arr2(5), i_arr3(10), i_res2(5,5))
        allocate(i_res3(10), i_res4(10), i_res5(5,5), i_arr4(10))
        i_res = 0.0e4
        i_arr = (/11.5e0, 22.5e0, 33.5e0, 44.5e0, 55.5e0/)
        i_arr2 = 0.0e0
        i_arr3 = 10.5d0
        i_res2 = 0.0d0
        i_res3 = 0.0d0
        i_res4 = 0.0d0
        i_res5 = 0.0d0
        i_arr4 = 0.0d0

        if ( .false. ) then
        else
          do concurrent (i=m:25:m, j=1:m:1, k=m:25:4)
            i_res(i/5) = abs(-1*i_arr(i/5))
          end do

          do x = 1,5
            if ( .not. precision_r4(i_res(x),i_arr(x)) ) then
              print *, "array variable assignment by function in do concurrent loop in else block returning bad result"
              print *, "x: ", x
              print *, "i_res: ", i_res
              error stop 1
            end if
          end do
        end if

        i_arr2 = (/10.5e0, 20.5e0, 30.5e0, 40.5e0, 50.5e0/)

        do concurrent (i = 1:5, j = 1:5, (mod(100.0e0,i_arr2(i)) > 15.1e0) .and. (mod(100.0e0,i_arr(j)) < 30.6e0))
          i_res2(i,j) = log10(real(10**j,8))
        end do

        i_res2_result(1,:) = 0.0d0 
        i_res2_result(2,:) = (/1.0d0, 2.0d0, 0.0d0, 4.0d0, 0.0d0/)
        i_res2_result(3,:) = 0.0d0
        i_res2_result(4,:) = (/1.0d0, 2.0d0, 0.0d0, 4.0d0, 0.0d0/)
        i_res2_result(5,:) = (/1.0d0, 2.0d0, 0.0d0, 4.0d0, 0.0d0/)
        do x = 1,5
          do y = 1,5
            if ( .not. precision_r8(i_res2(x,y),i_res2_result(x,y)) ) then
              print *, "do concurrent with index in mask produced incorrect results"
              print *, "x: ", x
              print *, "y: ", y
              print *, "i_res2: ", i_res2
              error stop 2
            end if
          end do
        end do

        i_res2 = 0.0d0

        do concurrent (i = 1:5, j=1:5, lvar .eqv. .true.)
          i_res2(i,j) = mod(100.5d0,real(i*j,8))
        end do

        i_res2_result(1,:) = (/0.5d0, 0.5d0, 1.5d0, 0.5d0, 0.5d0/)
        i_res2_result(2,:) = (/0.5d0, 0.5d0, 4.5d0, 4.5d0, 0.5d0/)
        i_res2_result(3,:) = (/1.5d0, 4.5d0, 1.5d0, 4.5d0, 10.5d0/)
        i_res2_result(4,:) = (/0.5d0, 4.5d0, 4.5d0, 4.5d0, 0.5d0/)
        i_res2_result(5,:) = (/0.5d0, 0.5d0, 10.5d0, 0.5d0, 0.5d0/)
        do x = 1,5
          do y = 1,5
            if ( .not. precision_r8(i_res2(x,y),i_res2_result(x,y)) ) then
              print *, "do concurrent with logical mask produced incorrect results"
              print *, "x: ", x
              print *, "y: ", y
              print *, "i_res2: ", i_res2
              error stop 3
            end if
          end do
        end do

        i_arr4 = (/4.0d0,9.0d0,16.0d0,25.0d0,36.0d0,49.0d0,64.0d0,81.0d0,100.0d0,121.0d0/)

        do concurrent (i = 1:10)
          i_res3(i) = sqrt(i_arr4(i))
          do concurrent (j = 10:1:-1)
            i_res4(j) = sqrt(i_arr4(j))
          end do
        end do

        i_res3_result = (/2.0d0,3.0d0,4.0d0,5.0d0,6.0d0,7.0d0,8.0d0,9.0d0,10.0d0,11.0d0/)
        i_res4_result = (/2.0d0,3.0d0,4.0d0,5.0d0,6.0d0,7.0d0,8.0d0,9.0d0,10.0d0,11.0d0/)
        do x = 1,10
          if ( (.not. precision_r8(i_res3(x),i_res3_result(x))) .or. &
              &(.not. precision_r8(i_res4(x),i_res4_result(x))) ) then
            print *, "2-level nested do concurrent produced incorrect results"
            print *, "x: ", x
            print *, "i_res3: ", i_res3
            print *, "i_res4: ", i_res4
            error stop 4
          end if
        end do

        i_res2 = 0.0d0

        do concurrent (i = 1:5, j = 1:5)
          i_res2(i,j) = i_arr3(i) * i
          do concurrent (k = 5:1:-1, l = 5:1:-1)
            i_res5(k,l) = sqrt(i_arr4(l))
          end do
        end do

        i_res2_result(1,:) = 10.5d0
        i_res2_result(2,:) = 21.0d0
        i_res2_result(3,:) = 31.5d0
        i_res2_result(4,:) = 42.0d0
        i_res2_result(5,:) = 52.5d0
        i_res5_result(1,:) = (/2.0d0,3.0d0,4.0d0,5.0d0,6.0d0/)
        i_res5_result(2,:) = (/2.0d0,3.0d0,4.0d0,5.0d0,6.0d0/)
        i_res5_result(3,:) = (/2.0d0,3.0d0,4.0d0,5.0d0,6.0d0/)
        i_res5_result(4,:) = (/2.0d0,3.0d0,4.0d0,5.0d0,6.0d0/)
        i_res5_result(5,:) = (/2.0d0,3.0d0,4.0d0,5.0d0,6.0d0/)
        do x = 1,5
          do y = 1,5
            if ( (.not. precision_r8(i_res2(x,y),i_res2_result(x,y))) .or. &
                &(.not. precision_r8(i_res5(x,y),i_res5_result(x,y))) ) then
              print *, "2-level nested do concurrent with multiple indices produced incorrect results"
              print *, "x: ", x
              print *, "y: ", y
              print *, "i_res2: ", i_res2
              print *, "i_res5: ", i_res5
              error stop 5
            end if
          end do
        end do

        i_res3 = 0.0d0
        i_res4 = 0.0d0
        i_arr3 = (/10.5d0,20.5d0,30.5d0,40.5d0,50.5d0,11.5d0,22.5d0,33.5d0,44.5d0,55.5d0/)

        do concurrent (i = 1:10, lvar .eqv. .true.)
          i_res3(i) = sqrt(i_arr4(i))
          do concurrent (j = 1:10, mod(100.0d0,i_arr3(j)) > 0.154d2)
            i_res4(j) = sqrt(i_arr4(j))
          end do
        end do

        i_res3_result = (/2.0d0,3.0d0,4.0d0,5.0d0,6.0d0,7.0d0,8.0d0,9.0d0,10.0d0,11.0d0/)
        i_res4_result = (/0.0d0,0.3d1,0.0d0,0.5d1,0.6d1,0.0d0,0.0d0,0.9d1,0.0d0,1.1d1/)
        do x = 1,10
          if ( (.not. precision_r8(i_res3(x),i_res3_result(x))) .or. &
              &(.not. precision_r8(i_res4(x),i_res4_result(x))) ) then
            print *, "2-level nested do concurrent with mask produced incorrect results"
            print *, "x: ", x
            print *, "i_res3: ", i_res3
            print *, "i_res4: ", i_res4
            error stop 6
          end if
        end do

        i_res2 = 0.0d0
        i_res5 = 0.0d0

        do concurrent (i = 1:5, j = 1:5, (abs(-1.0e0*i_arr(i)) > 12.0e0) .and. (abs(-1.0e0*i_arr(j)) < 50.0e0))
          i_res2(i,j) = mod(100.5_8,real(i*j,8))
          do concurrent (k = 1:5, l = 1:5, (abs(-1.0e0*i_arr(k)) > 26.5e0) .and. (abs(-1.0e0*i_arr(l)) < 40.0e0))
            i_res5(k,l) = sqrt(i_arr4(k))
          end do
        end do

        i_res2_result(1,:) = (/0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0/)
        i_res2_result(2,:) = (/0.5d0, 0.5d0, 4.5d0, 4.5d0, 0.0d0/)
        i_res2_result(3,:) = (/1.5d0, 4.5d0, 1.5d0, 4.5d0, 0.0d0/)
        i_res2_result(4,:) = (/0.5d0, 4.5d0, 4.5d0, 4.5d0, 0.0d0/)
        i_res2_result(5,:) = (/0.5d0, 0.5d0, 10.5d0, 0.5d0, 0.0d0/)
        i_res5_result(1,:) = 0.0d0
        i_res5_result(2,:) = 0.0d0
        i_res5_result(3,:) = (/4.0d0,4.0d0,4.0d0,0.0d0,0.0d0/)
        i_res5_result(4,:) = (/5.0d0,5.0d0,5.0d0,0.0d0,0.0d0/)
        i_res5_result(5,:) = (/6.0d0,6.0d0,6.0d0,0.0d0,0.0d0/)
        do x = 1,5
          do y = 1,5
            if ( (.not. precision_r8(i_res2(x,y),i_res2_result(x,y))) .or. &
                &(.not. precision_r8(i_res5(x,y),i_res5_result(x,y))) ) then
              print *, "2-level nested do concurrent with multiple indices produced incorrect results"
              print *, "x: ", x
              print *, "y: ", y
              print *, "i_res2: ", i_res2
              print *, "i_res5: ", i_res5
              error stop 7
            end if
          end do
        end do

        i_res3 = 0.0d0
        i_res4 = 0.0d0
        i_res2 = 0.0d0
        i_arr3 = (/10.0d0,20.0d0,30.0d0,40.0d0,50.0d0,60.0d0,70.0d0,80.0d0,90.0d0,100.0d0/)

        do concurrent (i = 1:m*2, lvar .eqv. .true.)
          i_res3(i) = sqrt(i_arr4(i)) 
          do concurrent (j = 100:1000:100, (abs(-1.0d0*i_arr3(j/100)) >= 30.0d0) .and. (abs(-1.0d0*i_arr3(j/100)) <= 80.0d0))
            i_res4(j/100) = sqrt(i_arr4(j/100))
            do concurrent (k = 1:5, l = 1:5)
              i_res2(k,l) = mod(100.5d0,real(k*l,8))
            end do
          end do
        end do

        i_res3_result = (/2.0d0,3.0d0,4.0d0,5.0d0,6.0d0,7.0d0,8.0d0,9.0d0,10.0d0,11.0d0/)
        do x = 1,10
          if ( .not. precision_r8(i_res3(x),i_res3_result(x)) ) then
            print *, "3-level nested do concurrent with multiple indices and masks produced incorrect results"
            print *, "failure in first, outer-most loop"
            print *, "x: ", x
            print *, "i_res3: ", i_res3
            error stop 8
          end if
        end do

        i_res4_result = (/0.0d0,0.0d0,4.0d0,5.0d0,6.0d0,7.0d0,8.0d0,9.0d0,0.0d0,0.0d0/)
        do x = 1,10
          if ( .not. precision_r8(i_res4(x),i_res4_result(x)) ) then
            print *, "3-level nested do concurrent with multiple indices and masks produced incorrect results"
            print *, "failure in second, inner loop"
            print *, "x: ", x
            print *, "i_res4: ", i_res4
            error stop 9
          end if
        end do

        i_res2_result(1,:) = (/0.5d0, 0.5d0, 1.5d0, 0.5d0, 0.5d0/) 
        i_res2_result(2,:) = (/0.5d0, 0.5d0, 4.5d0, 4.5d0, 0.5d0/)
        i_res2_result(3,:) = (/1.5d0, 4.5d0, 1.5d0, 4.5d0, 10.5d0/)
        i_res2_result(4,:) = (/0.5d0, 4.5d0, 4.5d0, 4.5d0, 0.5d0/)
        i_res2_result(5,:) = (/0.5d0, 0.5d0, 10.5d0, 0.5d0, 0.5d0/)
        do x = 1,5
          do y = 1,5
            if ( .not. precision_r8(i_res2(x,y),i_res2_result(x,y)) ) then
              print *, "3-level nested do concurrent with multiple indices and masks produced incorrect results"
              print *, "failure in third, inner-most loop"
              print *, "x: ", x
              print *, "y: ", y
              print *, "i_res2: ", i_res2
              error stop 10
            end if
          end do
        end do

      end
