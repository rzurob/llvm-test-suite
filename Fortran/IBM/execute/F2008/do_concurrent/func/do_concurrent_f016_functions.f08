!*******************************************************************************
!*
!============================================================================
!*
!============================================================================
!*
!*  DATE                       : 2015-06-30
!*
!*  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED :
!*  ADAPTED FROM               :
!*
!*  DESCRIPTION                : - DO CONCURRENT loops inside external
!*                                 subroutines and functions from external
!*                                 files
!*
!=============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890
subroutine sub1(i,j,l,i_arr)
  integer*1 :: i
  integer*2 :: j
  integer*4 :: k
  integer*8 :: l, x, y
  integer, parameter :: m = 5
  logical :: lvar = .true.
  double precision :: i_res(10)
  double precision, allocatable :: i_res2(:), i_arr2(:)
  double precision :: i_res_result(10), i_res2_result(10)
  real*8 :: i_res3(5,5)
  real*8 :: i_arr(10)
  real*8 :: i_res3_result(5,5)

  logical, external :: precision_r8

  allocate(i_res2(10), i_arr2(10))

  i_res = 0.0d0
  i_res2 = 0.0d0
  i_res3 = 0.0d0
  i_arr2 = (/4.0d0,9.0d0,16.0d0,25.0d0,36.0d0,49.0d0,64.0d0,81.0d0,100.0d0,121.0d0/)

  do concurrent (i = 1:m*2, lvar .eqv. .true.)
    i_res(i) = abs(i_arr2(i))
    do concurrent (j = 100:1000:100, (abs(-1.0d0*i_arr(j/100)) >= 30.0d0) .and. (abs(-1.0d0*i_arr(j/100)) <= 80.0d0))
      i_res2(j/100) = sqrt(i_arr2(j/100))
      do concurrent (k = 1:5, l = 1:5)
        i_res3(k,l) = mod(100.5d0,real(k*l,8))
      end do
    end do
  end do

  i_res_result = (/4.0d0,9.0d0,16.0d0,25.0d0,36.0d0,49.0d0,64.0d0,81.0d0,100.0d0,121.0d0/)
  i_res2_result = (/0.0d0,0.0d0,4.0d0,5.0d0,6.0d0,7.0d0,8.0d0,9.0d0,0.0d0,0.0d0/)
  i_res3_result(1,:) = (/0.5d0,0.5d0,1.5d0,0.5d0,0.5d0/)
  i_res3_result(2,:) = (/0.5d0,0.5d0,4.5d0,4.5d0,0.5d0/)
  i_res3_result(3,:) = (/1.5d0,4.5d0,1.5d0,4.5d0,10.5d0/)
  i_res3_result(4,:) = (/0.5d0,4.5d0,4.5d0,4.5d0,0.5d0/)
  i_res3_result(5,:) = (/0.5d0,0.5d0,10.5d0,0.5d0,0.5d0/)
  do x = 1,10
    if ( .not. precision_r8(i_res(x),i_res_result(x)) ) then
      print *, "Failure in sub1, 3-level nested DO CONCURRENT loop with scalar-mask-expr"
      print *, "x: ", x
      print *, "i_res: ", i_res
      error stop 10
    end if
  end do
  do x = 1,10
    if ( .not. precision_r8(i_res2(x),i_res2_result(x)) ) then
      print *, "Failure in sub1, 3-level nested DO CONCURRENT loop with scalar-mask-expr"
      print *, "x: ", x
      print *, "i_res2: ", i_res2
      error stop 11
    end if
  end do
  do x = 1,5
    do y = 1,5
      if ( .not. precision_r8(i_res3(x,y),i_res3_result(x,y)) ) then
        print *, "Failure in sub1, 3-level nested DO CONCURRENT loop with scalar-mask-expr"
        print *, "x: ", x
        print *, "y: ", y
        print *, "i_res3: ", i_res3
        error stop 12
      end if
    end do
  end do
end subroutine

subroutine sub4 ()
  integer*1 :: i
  integer*2 :: j
  integer*4 :: k, x, y
  integer*8 :: l
  real*8 :: i_res(5,5)
  double precision, allocatable :: i_res2(:,:), i_arr(:)
  double precision :: i_res2_result(5,5)
  real*8, allocatable :: i_arr2(:)
  real*8 :: i_res_result(5,5)

  logical, external :: precision_r8

  allocate(i_res2(5,5), i_arr(10), i_arr2(10))

  i_res = 0.0d0
  i_res2 = 0.0d0
  i_arr = (/4.0d0,9.0d0,16.0d0,25.0d0,36.0d0,49.0d0,64.0d0,81.0d0,100.0d0,121.0d0/)
  i_arr2 = 10.5d0

  do concurrent (i = 1:5, j = 1:5)
    i_res(i,j) = i_arr2(i) * i
    do concurrent (k = 5:1:-1, l = 5:1:-1)
      i_res2(k,l) = sqrt(i_arr(l))
    end do
  end do

  i_res_result(1,:) = 10.5d0
  i_res_result(2,:) = 21.0d0
  i_res_result(3,:) = 31.5d0
  i_res_result(4,:) = 42.0d0
  i_res_result(5,:) = 52.5d0
  i_res2_result(1,:) = (/2.0d0,3.0d0,4.0d0,5.0d0,6.0d0/)
  i_res2_result(2,:) = (/2.0d0,3.0d0,4.0d0,5.0d0,6.0d0/)
  i_res2_result(3,:) = (/2.0d0,3.0d0,4.0d0,5.0d0,6.0d0/)
  i_res2_result(4,:) = (/2.0d0,3.0d0,4.0d0,5.0d0,6.0d0/)
  i_res2_result(5,:) = (/2.0d0,3.0d0,4.0d0,5.0d0,6.0d0/)
  do x = 1,5
    do y = 1,5
      if ( .not. precision_r8(i_res(x,y),i_res_result(x,y)) ) then
        print *, "Failure in sub4, 2-level nested DO CONCURRENT loop"
        print *, "x: ", x
        print *, "y: ", y
        print *, "i_res: ", i_res
        error stop 17
      end if
    end do
  end do
  do x = 1,5
    do y = 1,5
      if ( .not. precision_r8(i_res2(x,y),i_res2_result(x,y)) ) then
        print *, "Failure in sub4, 2-level nested DO CONCURRENT loop"
        print *, "x: ", x
        print *, "y: ", y
        print *, "i_res2: ", i_res2
        error stop 18
      end if
    end do
  end do
end subroutine

real*8 function func2(j,l,i_arr)
  integer*1 :: i
  integer*2 :: j
  integer*4 :: k
  integer*8 :: l
  real*8 :: i_res(5,5)
  real*4 :: i_arr(5)
  double precision, allocatable :: i_res2(:,:), i_arr2(:)
  real*8 :: func2(5,5)

  allocate(i_res2(5,5),i_arr2(10))

  i_res = 0.0d0
  i_res2 = 0.0d0
  i_arr2 = (/4.0d0,9.0d0,16.0d0,25.0d0,36.0d0,49.0d0,64.0d0,81.0d0,100.0d0,121.0d0/)

  do concurrent (i = 1:5, j = 1:5, (abs(-1.0e0*i_arr(i)) > 12.0e0) .and. (abs(-1.0e0*i_arr(j)) < 50.0e0))
    i_res(i,j) = mod(100.5d0,real(i*j,8))
    do concurrent (k = 1:5, l = 1:5, (abs(-1.0e0*i_arr(k)) > 26.5e0) .and. (abs(-1.0e0*i_arr(l)) < 40.0e0))
      i_res2(k,l) = sqrt(i_arr2(k))
    end do
  end do

  func2 = i_res2
end function

double precision function func3(i_arr2)
  integer*1 :: i
  integer*2 :: j
  logical :: lvar = .true.
  double precision :: i_res(10)
  double precision, allocatable :: i_res2(:), i_arr(:)
  real*8 :: i_arr2(10)
  double precision :: func3(10)

  allocate(i_res2(10), i_arr(10))

  i_res = 0.0d0
  i_res2 = 0.0d0
  i_arr = (/4.0d0,9.0d0,16.0d0,25.0d0,36.0d0,49.0d0,64.0d0,81.0d0,100.0d0,121.0d0/)

  do concurrent (i = 1:10, lvar .eqv. .true.)
    i_res(i) = sqrt(i_arr(i))
    do concurrent (j = 1:10, mod(100.0d0,i_arr2(j)) > 0.154d2)
      i_res2(j) = sqrt(i_arr(j))
    end do
  end do

  func3 = i_res2
end function

subroutine sub2(j,l,i_arr)
  integer*1 :: i, x, y
  integer*2 :: j
  integer*4 :: k
  integer*8 :: l

  real*8 :: i_res(5,5)
  real*8 :: i_res_result(5,5)
  real*4 :: i_arr(5)
  double precision, allocatable :: i_res2(:,:), i_arr2(:)
  double precision :: i_res2_result(5,5)

  logical, external :: precision_r8

  allocate(i_res2(5,5),i_arr2(10))

  i_res = 0.0d0
  i_res2 = 0.0d0
  i_arr2 = (/4.0d0,9.0d0,16.0d0,25.0d0,36.0d0,49.0d0,64.0d0,81.0d0,100.0d0,121.0d0/)

  do concurrent (i = 1:5, j = 1:5, (abs(-1.0e0*i_arr(i)) > 12.0e0) .and. (abs(-1.0e0*i_arr(j)) < 50.0e0))
    i_res(i,j) = mod(100.5d0,real(i*j,8))
    do concurrent (k = 1:5, l = 1:5, (abs(-1.0e0*i_arr(k)) > 26.5e0) .and. (abs(-1.0e0*i_arr(l)) < 40.0e0))
      i_res2(k,l) = sqrt(i_arr2(k))
    end do
  end do

  i_res_result(1,:) = 0.0d0
  i_res_result(2,:) = (/0.5d0,0.5d0,4.5d0,4.5d0,0.0d0/)
  i_res_result(3,:) = (/1.5d0,4.5d0,1.5d0,4.5d0,0.0d0/)
  i_res_result(4,:) = (/0.5d0,4.5d0,4.5d0,4.5d0,0.0d0/)
  i_res_result(5,:) = (/0.5d0,0.5d0,10.5d0,0.5d0,0.0d0/)
  i_res2_result(1,:) = 0.0d0
  i_res2_result(2,:) = 0.0d0
  i_res2_result(3,:) = (/4.0d0,4.0d0,4.0d0,0.0d0,0.0d0/)
  i_res2_result(4,:) = (/5.0d0,5.0d0,5.0d0,0.0d0,0.0d0/)
  i_res2_result(5,:) = (/6.0d0,6.0d0,6.0d0,0.0d0,0.0d0/)
  do x = 1,5
    do y = 1,5
      if ( .not. precision_r8(i_res(x,y),i_res_result(x,y)) ) then
        print *, "Failure in sub2, 2-level nested DO CONCURRENT loop with scalar-mask-expr"
        print *, "x: ", x
        print *, "y: ", y
        print *, "i_res: ", i_res
        error stop 13
      end if
    end do
  end do
  do x = 1,5
    do y = 1,5
      if ( .not. precision_r8(i_res2(x,y),i_res2_result(x,y)) ) then
        print *, "Failure in sub2, 2-level nested DO CONCURRENT loop with scalar-mask-expr"
        print *, "x: ", x
        print *, "y: ", y
        print *, "i_res2: ", i_res2
        error stop 14
      end if
    end do
  end do
end subroutine

subroutine sub3(i_arr2)
  integer*1 :: i
  integer*2 :: j, x
  logical :: lvar = .true.
  double precision :: i_res(10)
  double precision, allocatable :: i_res2(:), i_arr(:)
  double precision :: i_res_result(10), i_res2_result(10)
  real*8 :: i_arr2(10)

  logical, external :: precision_r8

  allocate(i_res2(10), i_arr(10))

  i_res = 0.0d0
  i_res2 = 0.0d0
  i_arr = (/4.0d0,9.0d0,16.0d0,25.0d0,36.0d0,49.0d0,64.0d0,81.0d0,100.0d0,121.0d0/)

  do concurrent (i = 1:10, lvar .eqv. .true.)
    i_res(i) = sqrt(i_arr(i))
    do concurrent (j = 1:10, mod(100.0d0,i_arr2(j)) > 0.154d2)
      i_res2(j) = sqrt(i_arr(j))
    end do
  end do

  i_res_result = (/2.0d0,3.0d0,4.0d0,5.0d0,6.0d0,7.0d0,8.0d0,9.0d0,10.0d0,11.0d0/)
  i_res2_result = (/0.0d0,3.0d0,0.0d0,5.0d0,6.0d0,0.0d0,0.0d0,9.0d0,0.0d0,11.0d0/)
  do x = 1,10
    if ( .not. precision_r8(i_res(x),i_res_result(x)) ) then
      print *, "Failure in sub3, 2-level nested DO CONCURRENT loop with scalar-mask-expr"
      print *, "x: ", x
      print *, "i_res: ", i_res
      error stop 15
    end if
  end do
  do x = 1,10
    if ( .not. precision_r8(i_res2(x),i_res2_result(x)) ) then
      print *, "Failure in sub3, 2-level nested DO CONCURRENT loop with scalar-mask-expr"
      print *, "x: ", x
      print *, "i_res2: ", i_res2
      error stop 16
    end if
  end do
end subroutine

subroutine sub5(i,j)
  integer*1 :: i, x, y
  integer*2 :: j
  real*4, allocatable :: i_arr(:)
  real*4 :: i_arr2(5), value = 15.1e0
  real*8 :: i_res(5,5)
  real*8 :: i_res_result(5,5)

  logical, external :: precision_r8

  allocate(i_arr(5))

  i_arr = (/11.5e0, 22.5e0, 33.5e0, 44.5e0, 55.5e0/)
  i_arr2 = (/10.5e0, 20.5e0, 30.5e0, 40.5e0, 50.5e0/)
  i_res = 0.0d0

  do concurrent (i = 1:5, j = 1:5, (mod(100.0e0,i_arr2(i)) > value) .and. (mod(100.0e0,i_arr(j)) < 30.6e0))
    i_res(i,j) = log10(real(10**j,8))
  end do

  i_res_result(1,:) = 0.0d0
  i_res_result(2,:) = (/1.0d0,2.0d0,0.0d0,4.0d0,0.0d0/)
  i_res_result(3,:) = 0.0d0
  i_res_result(4,:) = (/1.0d0,2.0d0,0.0d0,4.0d0,0.0d0/)
  i_res_result(5,:) = (/1.0d0,2.0d0,0.0d0,4.0d0,0.0d0/)
  do x = 1,5
    do y = 1,5
      if ( .not. precision_r8(i_res(x,y),i_res_result(x,y)) ) then
        print *, "Failure in sub5, 1-level DO CONCURRENT loop with scalar-mask-expr"
        print *, "x: ", x
        print *, "y: ", y
        print *, "i_res: ", i_res
        error stop 19
      end if
    end do
  end do
end subroutine

real*8 function func1(i,j,l,i_arr)
  integer*1 :: i
  integer*2 :: j
  integer*4 :: k
  integer*8 :: l
  integer, parameter :: m = 5
  logical :: lvar = .true.
  double precision :: i_res(10)
  double precision, allocatable :: i_res2(:), i_arr2(:)
  real*8 :: i_res3(5,5)
  real*8 :: i_arr(10)
  real*8 :: func1(5,5)

  allocate(i_res2(10), i_arr2(10))

  i_res = 0.0d0
  i_res2 = 0.0d0
  i_res3 = 0.0d0
  i_arr2 = (/4.0d0,9.0d0,16.0d0,25.0d0,36.0d0,49.0d0,64.0d0,81.0d0,100.0d0,121.0d0/)

  do concurrent (i = 1:m*2, lvar .eqv. .true.)
    i_res(i) = abs(i_arr2(i))
    do concurrent (j = 100:1000:100, (abs(-1.0d0*i_arr(j/100)) >= 30.0d0) .and. (abs(-1.0d0*i_arr(j/100)) <= 80.0d0))
      i_res2(j/100) = sqrt(i_arr2(j/100))
      do concurrent (k = 1:5, l = 1:5)
        i_res3(k,l) = mod(100.5d0,real(k*l,8))
      end do
    end do
  end do

  func1 = i_res3
end function

real*8 function func4()
  integer*1 :: i
  integer*2 :: j
  integer*4 :: k
  integer*8 :: l
  real*8 :: i_res(5,5)
  double precision, allocatable :: i_res2(:,:), i_arr(:)
  real*8, allocatable :: i_arr2(:)
  real*8 :: func4(5,5)

  allocate(i_res2(5,5), i_arr(10), i_arr2(10))

  i_res = 0.0d0
  i_res2 = 0.0d0
  i_arr = (/4.0d0,9.0d0,16.0d0,25.0d0,36.0d0,49.0d0,64.0d0,81.0d0,100.0d0,121.0d0/)
  i_arr2 = 10.5d0

  do concurrent (i = 1:5, j = 1:5)
    i_res(i,j) = i_arr2(i) * i
    do concurrent (k = 5:1:-1, l = 5:1:-1)
      i_res2(k,l) = sqrt(i_arr(l))
    end do
  end do

  func4 = i_res
end function

real*8 function func5(i,j)
  integer*1 :: i
  integer*2 :: j
  real*4, allocatable :: i_arr(:)
  real*4 :: i_arr2(5), value = 15.1e0
  real*8 :: i_res(5,5), func5(5,5)

  allocate(i_arr(5))

  i_arr = (/11.5e0, 22.5e0, 33.5e0, 44.5e0, 55.5e0/)
  i_arr2 = (/10.5e0, 20.5e0, 30.5e0, 40.5e0, 50.5e0/)
  i_res = 0.0d0

  do concurrent (i = 1:5, j = 1:5, (mod(100.0e0,i_arr2(i)) > value) .and. (mod(100.0e0,i_arr(j)) < 30.6e0))
    i_res(i,j) = log10(real(10**j,8))
  end do

  func5 = i_res
end function