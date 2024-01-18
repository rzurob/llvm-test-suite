!*******************************************************************************
!*
!============================================================================
!*
!============================================================================
!*
!*  DATE                       : 2015-07-24
!*
!*  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT (F2008 extension)
!*
!*  DESCRIPTION                : - Various kinds of real pointers and double
!*                                 pointers in DO CONCURRENT loops including
!*                                 nested DO CONCURRENT loops
!*                               - concurrent-limit contains a variable with
!*                                 the parameter attribute
!*                               - scalar-mask-expr contains real pointers and
!*                                 logical pointers
!*                               - Use of pointers to procedures inside DO
!*                                 CONCURRENT loops with pointers as arguments
!*                               - Use of pointers to procedures in the
!*                                 scalar-mask-expr with pointers as arguments
!*
!=============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890
      module do_concurrent_module
        integer, parameter :: m = 5

        contains
          pure function func1(dp_var)
            double precision, intent(in) :: dp_var
            double precision :: func1

            func1 = sqrt(dp_var)
          end function
      end module

      program main
        use do_concurrent_module

        implicit none

        interface
          pure function func3_r8(r8_var1, r8_var2)
            real*8, intent(in) :: r8_var1, r8_var2
            real*8 :: func3_r8
          end function

          pure function func3_r4(r4_var1, r4_var2)
            real*4, intent(in) :: r4_var1, r4_var2
            real*4 :: func3_r4
          end function
        end interface

        logical, external :: precision_r4, precision_r8

        integer*1 :: i = 100, x
        integer*2 :: j = 200
        integer*8 :: k = 300
        integer :: l = 400, y
        real*4, allocatable, target :: i_res(:), i_arr(:)
        real*4, target :: i_arr2(5)
        real*8, allocatable, target :: i_arr3(:)
        real*8, target :: i_res2(5,5)
        real*8 :: i_res2_result(5,5)
        double precision, target :: i_res3(10)
        double precision, allocatable, target :: i_res4(:), i_res5(:,:)
        double precision :: i_res3_result(10), i_res4_result(10), i_res5_result(5,5)
        double precision, allocatable, target :: i_arr4(:)
        logical, target :: lvar = .true.
        real*4, target :: value = 0.0e0

        real*4, pointer :: p_i_res(:), p_i_arr(:), p_i_arr2(:)
        real*8, pointer :: p_i_arr3(:), p_i_res2(:,:)
        double precision, pointer :: p_i_res3(:), p_i_res4(:), p_i_res5(:,:)
        double precision, pointer :: p_i_arr4(:)
        logical, pointer :: p_lvar
        real*4, pointer :: p_value

        procedure (sub1), pointer :: p_sub1 => null()
        procedure (func1), pointer :: p_func1 => null()
        procedure (func2_r4), pointer :: p_func2_r4 => null()
        procedure (func2_r8), pointer :: p_func2_r8 => null()
        procedure (func3_r4), pointer :: p_func3_r4 => null()
        procedure (func3_r8), pointer :: p_func3_r8 => null()

        p_sub1 => sub1
        p_func1 => func1
        p_func2_r4 => func2_r4
        p_func2_r8 => func2_r8
        p_func3_r4 => func3_r4
        p_func3_r8 => func3_r8

        allocate(i_res(5), i_arr(5), i_arr3(10))
        allocate(i_res4(10), i_res5(5,5), i_arr4(10))
        i_res = 0.0e0
        i_arr = (/11.5e0, 22.5e0, 33.5e0, 44.5e0, 55.5e0/)
        i_arr2 = 0.0e0
        i_arr3 = 10.5d0
        i_res2 = 0.0d0
        i_res3 = 0.0d0
        i_res4 = 0.0d0
        i_res5 = 0.0d0
        i_arr4 = 0.0d0

        p_i_res => i_res
        p_i_arr =>  i_arr
        p_i_arr2 => i_arr2
        p_i_arr3 => i_arr3
        p_i_res2 => i_res2
        p_i_res3 => i_res3
        p_i_res4 => i_res4
        p_i_res5 => i_res5
        p_i_arr4 => i_arr4
        p_lvar => lvar
        p_value => value

        if ( .false. ) then
        else
          do concurrent (i=m:25:m, j=1:m:1, k=m:25:4)
            p_i_res(i/5) = p_func2_r4(-1*p_i_arr(i/5))
            call p_sub1()
          end do

          do x = 1,5
            if ( .not. precision_r4(i_res(x),i_arr(x)) ) then
              print *, "array variable assignment by function in do concurrent loop in else block returning bad result"
              print *, "i_res: ", i_res
              error stop 1
            end if
          end do
        end if

        i_arr2 = (/10.5e0, 20.5e0, 30.5e0, 40.5e0, 50.5e0/)
        value = 15.1e0

        do concurrent (i = 1:5, j = 1:5, (p_func3_r4(100.0e0,p_i_arr2(i)) > p_value) .and. (p_func3_r4(100.0e0,p_i_arr(j)) < 30.6e0))
          call p_sub1()
          p_i_res2(i,j) = log10(real(10**j,8))
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

        do concurrent (i = 1:5, j=1:5, p_lvar .eqv. .true.)
          p_i_res2(i,j) = p_func3_r8(100.5d0,real(i*j,8))
          call p_sub1()
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
          p_i_res3(i) = p_func1(p_i_arr4(i))
          call p_sub1()
          do concurrent (j = 10:1:-1)
            p_i_res4(j) = p_func1(p_i_arr4(j))
            call p_sub1()
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
          call p_sub1()
          p_i_res2(i,j) = p_i_arr3(i) * i
          do concurrent (k = 5:1:-1, l = 5:1:-1)
            call p_sub1()
            p_i_res5(k,l) = p_func1(p_i_arr4(l))
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
              print *, "do concurrent with logical mask produced incorrect results"
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

        do concurrent (i = 1:10, p_lvar .eqv. .true.)
          p_i_res3(i) = p_func1(p_i_arr4(i))
          call p_sub1()
          do concurrent (j = 1:10, p_func3_r8(100.0d0,p_i_arr3(j)) > 0.154d2)
            call p_sub1()
            p_i_res4(j) = p_func1(p_i_arr4(j))
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

        do concurrent (i = 1:5, j = 1:5, (p_func2_r4(-1.0e0*p_i_arr(i)) > 12.0e0) .and. (p_func2_r4(-1.0e0*p_i_arr(j)) < 50.0e0))
          p_i_res2(i,j) = p_func3_r8(100.5d0,real(i*j,8))
          call p_sub1()
          do concurrent (k = 1:5, l = 1:5, (p_func2_r4(-1.0e0*p_i_arr(k)) > 26.5e0) .and. (p_func2_r4(-1.0e0*p_i_arr(l)) < 40.0e0))
            p_i_res5(k,l) = p_func1(p_i_arr4(k))
            call p_sub1()
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

        do concurrent (i = 1:m*2, p_lvar .eqv. .true.)
          call p_sub1()
          p_i_res3(i) = p_func1(p_i_arr4(i))
          do concurrent (j = 100:1000:100, (p_func2_r8(-1.0d0*p_i_arr3(j/100)) >= 30.0d0) .and. (p_func2_r8(-1.0d0*p_i_arr3(j/100)) <= 80.0d0))
            p_i_res4(j/100) = p_func1(p_i_arr4(j/100))
            call p_sub1()
            do concurrent (k = 1:5, l = 1:5)
              call p_sub1()
              p_i_res2(k,l) = p_func3_r8(100.5d0,real(k*l,8))
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
          if ( .not. precision_r8(i_res3(x),i_res3_result(x)) ) then
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

        contains
          pure subroutine sub1()
            integer*1 :: result

            result = 100
          end subroutine

          pure function func2_r4(r4_var)
            real*4, intent(in) :: r4_var
            real*4 :: func2_r4

            if (r4_var < 0.0e0) then
              func2_r4 = -1.0e0 * r4_var
            else
              func2_r4 = r4_var
            end if
          end function

          pure function func2_r8(r8_var)
            real*8, intent(in) :: r8_var
            real*8 :: func2_r8

            if (r8_var < 0.0d0) then
              func2_r8 = -1.0d0 * r8_var
            else
              func2_r8 = r8_var
            end if
          end function
      end program

pure function func3_r8(r8_var1, r8_var2)
  real*8, intent(in) :: r8_var1, r8_var2
  real*8 :: func3_r8

  func3_r8 = mod(r8_var1, r8_var2)
end function

pure function func3_r4(r4_var1, r4_var2)
  real*4, intent(in) :: r4_var1, r4_var2
  real*4 :: func3_r4

  func3_r4 = mod(r4_var1, r4_var2)
end function
