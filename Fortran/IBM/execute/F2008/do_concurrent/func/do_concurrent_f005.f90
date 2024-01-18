!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : F2008/do_concurrent/func/do_concurrent_f005.f
!*
!*  PROGRAMMER                 : Nicole Negherbon 
!*  DATE                       : 2015-03-26
!*
!*  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT (F2008 extension)
!*
!*  DESCRIPTION                : - Various kinds of integer pointers in DO 
!*                                 CONCURRENT loops including nested DO 
!*                                 CONCURRENT loops
!*                               - concurrent-limit contains a variable with the 
!*                                 parameter attribute
!*                               - scalar-mask-expr contains integer and logical
!*                                 pointers
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890
      program main
        implicit none

        integer*1 :: i = 100
        integer*2 :: j = 200
        integer*4 :: k = 300
        integer*8 :: l = 400
        integer, parameter :: m = 5
        integer, target :: i_res(5) = 0, i_arr(5) = (/11, 22, 33, 44, 55/), i_arr2(5) = 0
        integer*4, allocatable, target :: i_arr3(:), i_res2(:,:)
        integer*2, target :: i_res3(10) = 0, i_res4(10) = 0, i_res5(5,5) = 0
        integer*1, target :: value = 0
        logical, target :: lvar = .true.
        
        integer, pointer :: p_i_res(:), p_i_arr(:), p_i_arr2(:)
        integer*4, pointer :: p_i_arr3(:), p_i_res2(:,:)
        integer*2, pointer :: p_i_res3(:), p_i_res4(:), p_i_res5(:,:)
        integer*1, pointer :: p_value
        logical, pointer :: p_lvar
        
        allocate(i_arr3(10), i_res2(5,5))
        i_arr3 = 10
        i_res2 = 0

        p_i_res => i_res
        p_i_arr => i_arr
        p_i_arr2 => i_arr2
        p_i_arr3 => i_arr3
        p_i_res2 => i_res2
        p_i_res3 => i_res3
        p_i_res4 => i_res4
        p_i_res5 => i_res5
        p_value => value
        p_lvar => lvar

        if ( .false. ) then
        else
          do concurrent (i=m:25:m, j=1:m, k=5:m*5:4) 
            p_i_res(i/5) = p_i_arr(i/5)
          end do

          if ( any(p_i_res .ne. p_i_arr) ) then
            print *, "array variable assignment in do concurrent loop in else block returning bad result"
            print *, "p_i_res: ", p_i_res
            error stop 1 
          end if
        end if

        i_arr2 = (/10, 20, 30, 40, 50/)
        value = 20

        do concurrent (i = 1:5, j = 1:5, (p_i_arr2(i) > p_value) .and. (p_i_arr(j) < 50))
          p_i_res2(i,j) = p_i_arr(j)*i
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
          error stop 2
        end if

        i_res2 = 0 

        do concurrent (i = 1:5, j=1:5, p_lvar .eqv. .true.)
          p_i_res2(i,j) = p_i_arr(i) + 10
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
          error stop 3 
        end if

        do concurrent (i = 1:10)
          p_i_res3(i) = i
          do concurrent (j = 1:10)
            p_i_res4(j) = j*10 
          end do
        end do      

        if ( any(i_res3 .ne. (/1,2,3,4,5,6,7,8,9,10/)) .or. &
            &any(i_res4 .ne. (/10,20,30,40,50,60,70,80,90,100/)) ) then
          print *, "2-level nested do concurrent produced incorrect results"
          print *, "i_res3: ", i_res3
          print *, "i_res4: ", i_res4
          error stop 4 
        end if

        i_res2 = 0

        do concurrent (i = 1:5, j = 1:5)
          p_i_res2(i,j) = i + j
          do concurrent (k = 1:5, l = 1:5)
            p_i_res5(k,l) = k * l
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
          error stop 5
        end if 

        i_res3 = 0
        i_res4 = 0
        i_arr3 = (/10,20,30,40,50,60,70,80,90,100/)

        do concurrent (i = 1:10, p_lvar .eqv. .true.)
          p_i_res3(i) = i
          do concurrent (j = 10:100:10, p_i_arr3(j/10) > 10)
            p_i_res4(j/10) = j/10
          end do
        end do

        if ( any(i_res3 .ne. (/1,2,3,4,5,6,7,8,9,10/)) .or. &
            &any(i_res4 .ne. (/0,2,3,4,5,6,7,8,9,10/)) ) then
          print *, "2-level nested do concurrent with mask produced incorrect results"
          print *, "i_res3: ", i_res3
          print *, "i_res4: ", i_res4
          error stop 6
        end if 

        i_res2 = 0
        i_res5 = 0
        value = 10

        do concurrent (i = 1:m, j = 1:m, (p_i_arr(i) > (p_value + 2)) .and. (p_i_arr(j) < 50))
          p_i_res2(i,j) = i+j
          do concurrent (k = 1:5, l = 1:5, (p_i_arr(k) > 26) .and. (p_i_arr(l) < (p_value * 4)))
            p_i_res5(k,l) = k*l
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
          error stop 7 
        end if 

        i_res3 = 0
        i_res4 = 0
        i_res2 = 0
        i_arr3 = (/10,20,30,40,50,60,70,80,90,100/)
        value = 20

        do concurrent (i = 1:10, p_lvar .eqv. .true.)
          p_i_res3(i) = i
          do concurrent (j = 100:1000:100, (p_i_arr3(j/100) >= 30) .and. (p_i_arr3(j/100) <= (p_value * 4)))
            p_i_res4(j/100) = j
            do concurrent (k = 1:5, l = 1:5)
              p_i_res2(k,l) = k*l
            end do
          end do
        end do

        if ( any(i_res3 .ne. (/1,2,3,4,5,6,7,8,9,10/)) ) then
          print *, "3-level nested do concurrent with multiple indices and masks produced incorrect results"
          print *, "failure in first, outer-most loop"
          print *, "i_res3: ", i_res3
          error stop 8
        end if

        if ( any(i_res4 .ne. (/0,0,300,400,500,600,700,800,0,0/)) ) then
          print *, "3-level nested do concurrent with multiple indices and masks produced incorrect results"
          print *, "failure in second, inner loop"
          print *, "i_res4: ", i_res4
          error stop 9
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
          error stop 10
        end if
 
      end
