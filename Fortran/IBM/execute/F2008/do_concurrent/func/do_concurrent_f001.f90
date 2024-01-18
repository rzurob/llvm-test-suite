!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : F2008/do_concurrent/func/do_concurrent_f001.f
!*
!*  PROGRAMMER                 : Nicole Negherbon 
!*  DATE                       : 2015-02-26
!*
!*  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT (F2008 extension)
!*
!*  DESCRIPTION                : - Various kinds of integer variables in DO 
!*                                 CONCURRENT loops including nested DO 
!*                                 CONCURRENT loops 
!*                               - concurrent-limit contains a variable with the 
!*                                 parameter attribute
!*                               - scalar-mask-expr contains arrays of various 
!*                                 integer types and logicals
!*                               - index-name declared in DO CONCURRENT
!*                                 construct using parameter as kind definition
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890
      program main
        implicit none

        integer*1 :: i = 100
        integer*2 :: j = 200
        integer*4 :: k = 300
        integer*8 :: l = 400
        integer, parameter :: m = 5, n = 8
        integer :: i_res(5) = 0, i_arr(5) = (/ 11, 22, 33, 44, 55 /), i_arr2(5) = 0
        integer*4 :: i_arr3(10) = 10, i_res2(5,5) = 0
        integer*2 :: i_res3(10) = 0, i_res4(10) = 0, i_res5(5,5) = 0
        logical :: lvar = .true.

        do concurrent (i = 1:20:3, j = 1:300:10)
        end do 

        if (i .ne. 100) then
          print *, "do concurrent incrementer modified an external scope variable"
          print *, "i: ", i
          error stop 1
        end if

        if (j .ne. 200) then
          print *, "do concurrent incrementer modified an external scope variable"
          print *, "j: ", j
          error stop 2 
        else
          do concurrent (i=m:25:m, j=1:m, k=5:m*5:4) 
            i_res(i/5) = i_arr(i/5)
          end do

          if ( (i .ne. 100) .or. (j .ne. 200) .or. (k .ne. 300) ) then
            print *, "do concurrent incrementer modified an external scope variable in else block"
            print *, "i: ", i
            print *, "j: ", j
            print *, "k: ", k
            error stop 3
          end if

          if ( any(i_res .ne. i_arr) ) then
            print *, "array variable assignment in do concurrent loop in else block returning bad result"
            print *, "i_res: ", i_res
            error stop 4
          end if
        end if

        i_arr2 = (/10, 20, 30, 40, 50/)

        do concurrent (i = 1:5, j = 1:5, (i_arr2(i) > 20) .and. (i_arr(j) < 50))
          i_res2(i,j) = i_arr(j)*i
        end do

        if ( any(i_res2(1,:) .ne. 0) .or. &
            &any(i_res2(2,:) .ne. 0) .or. &
            &any(i_res2(3,:) .ne. (/33,66,99,132,0/)) .or. &
            &any(i_res2(4,:) .ne. (/44,88,132,176, 0/)) .or. &
            &any(i_res2(5,:) .ne. (/55,110,165,220,0/))) then
          print *, "do concurrent with index in mask produced incorrect results"
          print *, "i_res2(1,:): ", i_res2(1,:)
          print *, "i_res2(2,:): ", i_res2(2,:)
          print *, "i_res2(3,:): ", i_res2(3,:)
          print *, "i_res2(4,:): ", i_res2(4,:)
          print *, "i_res2(5,:): ", i_res2(5,:)
          error stop 6
        end if

        i_res2 = 0 

        do concurrent (i = 1:5, j=1:5, lvar .eqv. .true.)
          i_res2(i,j) = i_arr(i) + 10
        end do

        if ( any(i_res2(1,:) .ne. 21) .or. &
            &any(i_res2(2,:) .ne. 32) .or. &
            &any(i_res2(3,:) .ne. 43) .or. &
            &any(i_res2(4,:) .ne. 54) .or. &
            &any(i_res2(5,:) .ne. 65)) then
          print *, "do concurrent with logical mask produced incorrect results"
          print *, "i_res2(1,:): ", i_res2(1,:)
          print *, "i_res2(2,:): ", i_res2(2,:)
          print *, "i_res2(3,:): ", i_res2(3,:)
          print *, "i_res2(4,:): ", i_res2(4,:)
          print *, "i_res2(5,:): ", i_res2(5,:)
          error stop 7
        end if

        do concurrent (i = 1:10)
          i_res3(i) = i
          do concurrent (j = 1:10)
            i_res4(j) = j*10 
          end do
        end do      

        if ( any(i_res3 .ne. (/1,2,3,4,5,6,7,8,9,10/)) .or. &
            &any(i_res4 .ne. (/10,20,30,40,50,60,70,80,90,100/)) ) then
          print *, "2-level nested do concurrent produced incorrect results"
          print *, "i_res3: ", i_res3
          print *, "i_res4: ", i_res4
          error stop 8
        end if

        i_res2 = 0

        do concurrent (i = 1:5, j = 1:5)
          i_res2(i,j) = i + j
          do concurrent (k = 1:5, l = 1:5)
            i_res5(k,l) = k * l
          end do
        end do

        if ( any(i_res2(1,:) .ne. (/2,3,4,5,6/)) .or. &
            &any(i_res2(2,:) .ne. (/3,4,5,6,7/)) .or. &
            &any(i_res2(3,:) .ne. (/4,5,6,7,8/)) .or. &
            &any(i_res2(4,:) .ne. (/5,6,7,8,9/)) .or. &
            &any(i_res2(5,:) .ne. (/6,7,8,9,10/)) .or. &
            &any(i_res5(1,:) .ne. (/1,2,3,4,5/)) .or. &
            &any(i_res5(2,:) .ne. (/2,4,6,8,10/)) .or. &
            &any(i_res5(3,:) .ne. (/3,6,9,12,15/)) .or. &
            &any(i_res5(4,:) .ne. (/4,8,12,16,20/)) .or. &
            &any(i_res5(5,:) .ne. (/5,10,15,20,25/)) ) then
          print *, "2-level nested do concurrent with multiple indices produced incorrect results"
          print *, "i_res2(1,:): ", i_res2(1,:)
          print *, "i_res2(2,:): ", i_res2(2,:)
          print *, "i_res2(3,:): ", i_res2(3,:)
          print *, "i_res2(4,:): ", i_res2(4,:)
          print *, "i_res2(5,:): ", i_res2(5,:)
          print *, "i_res5(1,:): ", i_res5(1,:)
          print *, "i_res5(2,:): ", i_res5(2,:)
          print *, "i_res5(3,:): ", i_res5(3,:)
          print *, "i_res5(4,:): ", i_res5(4,:)
          print *, "i_res5(5,:): ", i_res5(5,:)
          error stop 9
        end if 

        i_res3 = 0
        i_res4 = 0
        i_arr3 = (/10,20,30,40,50,60,70,80,90,100/)

        do concurrent (i = 1:10, lvar .eqv. .true.)
          i_res3(i) = i
          do concurrent (j = 10:100:10, i_arr3(j/10) > 10)
            i_res4(j/10) = j/10
          end do
        end do

        if ( any(i_res3 .ne. (/1,2,3,4,5,6,7,8,9,10/)) .or. &
            &any(i_res4 .ne. (/0,2,3,4,5,6,7,8,9,10/)) ) then
          print *, "2-level nested do concurrent with mask produced incorrect results"
          print *, "i_res3: ", i_res3
          print *, "i_res4: ", i_res4
          error stop 10
        end if 

        i_res2 = 0
        i_res5 = 0

        do concurrent (i = 1:m, j = 1:m, (i_arr(i) > 12) .and. (i_arr(j) < 50))
          i_res2(i,j) = i+j
          do concurrent (k = 1:5, l = 1:5, (i_arr(k) > 26) .and. (i_arr(l) < 40))
            i_res5(k,l) = k*l
          end do
        end do

        if ( any(i_res2(1,:) .ne. 0) .or. &
            &any(i_res2(2,:) .ne. (/3,4,5,6,0/)) .or. &
            &any(i_res2(3,:) .ne. (/4,5,6,7,0/)) .or. &
            &any(i_res2(4,:) .ne. (/5,6,7,8,0/)) .or. &
            &any(i_res2(5,:) .ne. (/6,7,8,9,0/)) .or. &
            &any(i_res5(1,:) .ne. 0) .or. &
            &any(i_res5(2,:) .ne. 0) .or. &
            &any(i_res5(3,:) .ne. (/3,6,9,0,0/)) .or. &
            &any(i_res5(4,:) .ne. (/4,8,12,0,0/)) .or. &
            &any(i_res5(5,:) .ne. (/5,10,15,0,0/)) ) then
          print *, "2-level nested do concurrent with multiple indices produced incorrect results"
          print *, "i_res2(1,:): ", i_res2(1,:)
          print *, "i_res2(2,:): ", i_res2(2,:)
          print *, "i_res2(3,:): ", i_res2(3,:)
          print *, "i_res2(4,:): ", i_res2(4,:)
          print *, "i_res2(5,:): ", i_res2(5,:)
          print *, "i_res5(1,:): ", i_res5(1,:)
          print *, "i_res5(2,:): ", i_res5(2,:)
          print *, "i_res5(3,:): ", i_res5(3,:)
          print *, "i_res5(4,:): ", i_res5(4,:)
          print *, "i_res5(5,:): ", i_res5(5,:)
          error stop 11 
        end if 

        i_res3 = 0
        i_res4 = 0
        i_res2 = 0
        i_arr3 = (/10,20,30,40,50,60,70,80,90,100/)

        do concurrent (i = 1:10, lvar .eqv. .true.)
          i_res3(i) = i
          do concurrent (j = 100:1000:100, (i_arr3(j/100) >= 30) .and. (i_arr3(j/100) <= 80))
            i_res4(j/100) = j
            do concurrent (integer(n)::k = 1:5, o = 1:5)
              i_res2(k,o) = k*o
            end do
          end do
        end do

        if ( any(i_res3 .ne. (/1,2,3,4,5,6,7,8,9,10/)) ) then
          print *, "3-level nested do concurrent with multiple indices and masks produced incorrect results"
          print *, "failure in first, outer-most loop"
          print *, "i_res3: ", i_res3
          error stop 12
        end if

        if ( any(i_res4 .ne. (/0,0,300,400,500,600,700,800,0,0/)) ) then
          print *, "3-level nested do concurrent with multiple indices and masks produced incorrect results"
          print *, "failure in second, inner loop"
          print *, "i_res4: ", i_res4
          error stop 13
        end if

        if ( any(i_res2(1,:) .ne. (/1,2,3,4,5/)) .or. &
            &any(i_res2(2,:) .ne. (/2,4,6,8,10/)) .or. &
            &any(i_res2(3,:) .ne. (/3,6,9,12,15/)) .or. &
            &any(i_res2(4,:) .ne. (/4,8,12,16,20/)) .or. &
            &any(i_res2(5,:) .ne. (/5,10,15,20,25/)) ) then
          print *, "3-level nested do concurrent with multiple indices and masks produced incorrect results"
          print *, "failure in third, inner-most loop"
          print *, "i_res2(1,:): ", i_res2(1,:)
          print *, "i_res2(2,:): ", i_res2(2,:)
          print *, "i_res2(3,:): ", i_res2(3,:)
          print *, "i_res2(4,:): ", i_res2(4,:)
          print *, "i_res2(5,:): ", i_res2(5,:)
          error stop 14
        end if
 
      end
