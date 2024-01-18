!*******************************************************************************
!*
!============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL
!USE ONLY
!*
!============================================================================
!*
!*  TEST CASE NAME             : do_concurrent_f002.f
!*
!*  PROGRAMMER                 : Bernard Kan
!*  DATE                       : 2015-03-24
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED :
!*  ADAPTED FROM               :
!*
!*  DESCRIPTION                : - variables inside and outside of loops
!*                               - variables inside and outside nested loops
!*                               - functions inside loops/nested loops
!*                               - scalar-mask-expr is a pure subroutine
!*                               --- inside submodule subroutines
!*                               --- variable scope can be in: 
!*                                   - host
!*                                   - module
!*                                   - module subroutine
!*
!*                                       baseMod
!*                                          |
!*                                       subMod
!*                                       /    \
!*                                      /      \
!*                                     /        \
!*                                    /          \
!*                                   /            \
!*                                  /            subMod_rightChild
!*                                 /             /              \
!*                                /             /                \
!*                               /  subMod_rightChild_leftChild   \
!*                              /                                  \
!*                             /                     subMod_rightChild_rightChild
!*                    subMod_leftChild                          
!*                     /            \                            
!*  subMod_leftChild_leftChild subMod_leftChild_rightChild 
!=============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890
      module baseMod
        implicit none

        integer*1 :: i = 100
        integer*2 :: j = 200
        integer*8 :: k = 300
        integer :: l = 400
        integer, parameter :: m = 5
        real*4 :: i_res(5) = 0.0, i_arr(5) = (/ 11.5, 22.5, 33.5, 44.5, 55.5 /), i_arr2(5) = 0.0
        real*8 :: i_arr3(10) = 10.5, i_res2(5,5) = 0.0
        double precision :: i_res3(10) = 0.0d0, i_res4(10) = 0.0d0, i_res5(5,5) = 0.0d0
        double precision :: i_arr4(10) = 0.0d0
        logical :: lvar = .TRUE.

        interface

          ! module subroutine, defined as a module subroutine with do concurrent in its scope
          module subroutine do_test1()
          end subroutine do_test1

          ! module subroutine, defined as a module procedure with do concurrent in its scope
          module subroutine do_test2()
          end subroutine do_test2

          ! module subroutine, defined as a module procedure, calls a subroutine with do concurrent in the module scope
          module subroutine do_test3()
          end subroutine do_test3

          ! module function defined as a module function with do concurrent in its scope
          module logical function do_test4()
          end function do_test4

          ! module function defined as a module procedure with do concurrent in its scope
          module integer function do_test5()
          end function do_test5

          ! module function, defined as module function, calls a function with do concurrent in the module scope
          module real function do_test6()
          end function do_test6

          ! module function, defined as module procedure, calls a function with do concurrent in the module scope
          module complex function do_test7()
          end function do_test7

        end interface

        contains
          ! do concurrent in a function in module scope
          subroutine do_test0()
            if (j .ne. 200) then
              print *, "failure at start of test case"
              print *, "j: ", j
              error stop 1
            else
              do concurrent (i=m:25:m, j=1:m:1, k=m:25:4)
                i_res(i/5) = abs(-1*i_arr(i/5))
              end do

              if ( (i .ne. 100) .OR. (j .ne. 200) .OR. (k .ne. 300) ) then
                print *, "do concurrent incrementer modified an external scope variable in else block"
                print *, "i: ", i
                print *, "j: ", j
                print *, "k: ", k
                error stop 2
              end if
    
              if ( any(i_res .ne. i_arr) ) then
                print *, "array variable assignment by function in do concurrent loop in else block returning bad result"
                print *, "i_res: ", i_res
                error stop 3
              end if
            end if
          end subroutine do_test0
      end module baseMod

      submodule (baseMod) subMod
        contains
          module subroutine do_test1()
            i_arr2 = (/10.5, 20.5, 30.5, 40.5, 50.5/)
    
            do concurrent (i = 1:5, j = 1:5, mod(100.0,i_arr2(i)) > 15.1 .AND. mod(100.0,i_arr(j)) < 30.6)
              i_res2(i,j) = log10(real(10**j,8))
            end do
    
            if ( any(i_res2(1,:) .ne. 0.0) .OR. &
                &any(i_res2(2,:) .ne. (/1.0, 2.0, 0.0, 4.0, 0.0/)) .OR. &
                &any(i_res2(3,:) .ne. 0.0) .OR. &
                &any(i_res2(4,:) .ne. (/1.0, 2.0, 0.0, 4.0, 0.0/)) .OR. &
                &any(i_res2(5,:) .ne. (/1.0, 2.0, 0.0, 4.0, 0.0/))) then
              print *, "do concurrent with index in mask produced incorrect results"
              print *, "i_res2(1,:): ", i_res2(1,:)
              print *, "i_res2(2,:): ", i_res2(2,:)
              print *, "i_res2(3,:): ", i_res2(3,:)
              print *, "i_res2(4,:): ", i_res2(4,:)
              print *, "i_res2(5,:): ", i_res2(5,:)
              error stop 4
            end if
          end subroutine
      end submodule

      submodule (baseMod:subMod) subMod_leftChild
        contains
    
         module procedure do_test2
            i_res2 = 0.0
    
            do concurrent (i = 1:5, j=1:5, lvar .eqv. .TRUE.)
              i_res2(i,j) = mod(100.5_8,real(i*j,8))
            end do

            if ( any(i_res2(1,:) .ne. (/0.5, 0.5, 1.5, 0.5, 0.5/)) .OR. &
                &any(i_res2(2,:) .ne. (/0.5, 0.5, 4.5, 4.5, 0.5/)) .OR. &
                &any(i_res2(3,:) .ne. (/1.5, 4.5, 1.5, 4.5, 10.5/)) .OR. &
                &any(i_res2(4,:) .ne. (/0.5, 4.5, 4.5, 4.5, 0.5/)) .OR. &
                &any(i_res2(5,:) .ne. (/0.5, 0.5, 10.5, 0.5, 0.5/))) then
              print *, "do concurrent with logical mask produced incorrect results"
              print *, "i_res2(1,:): ", i_res2(1,:)
              print *, "i_res2(2,:): ", i_res2(2,:)
              print *, "i_res2(3,:): ", i_res2(3,:)
              print *, "i_res2(4,:): ", i_res2(4,:)
              print *, "i_res2(5,:): ", i_res2(5,:)
              error stop 5
            end if

          end procedure
      end submodule subMod_leftChild

      submodule (baseMod:subMod) subMod_rightChild
        contains
          module procedure do_test3
            call do_test3_helper
          end procedure

          subroutine do_test3_helper()
            i_arr4 = (/4.0d0,9.0d0,16.0d0,25.0d0,36.0d0,49.0d0,64.0d0,81.0d0,100.0d0,121.0d0/)

            do concurrent (i = 1:10)
              i_res3(i) = sqrt(i_arr4(i))
              do concurrent (j = 10:1:-1)
                i_res4(j) = sqrt(i_arr4(j))
              end do
            end do

            if ( any(i_res3 .ne.  (/2.0d0,3.0d0,4.0d0,5.0d0,6.0d0,7.0d0,8.0d0,9.0d0,10.0d0,11.0d0/)) .OR. &
                &any(i_res4 .ne.  (/2.0d0,3.0d0,4.0d0,5.0d0,6.0d0,7.0d0,8.0d0,9.0d0,10.0d0,11.0d0/)) ) then
              print *, "2-level nested do concurrent produced incorrect results"
              print *, "i_res3: ", i_res3
              print *, "i_res4: ", i_res4
              error stop 6
            end if

          end subroutine
      end submodule subMod_rightChild

      submodule (baseMod:subMod_leftChild) subMod_leftChild_leftChild
        contains
          module logical function do_test4()
            i_res2 = 0.0

            do concurrent (i = 1:5, j = 1:5)
              i_res2(i,j) = i_arr3(i) * i
              do concurrent (k = 5:1:-1, l = 5:1:-1)
                i_res5(k,l) = sqrt(i_arr4(l))
              end do
            end do

            if ( any(i_res2(1,:) .ne. 10.5) .OR. &
                &any(i_res2(2,:) .ne. 21.0) .OR. &
                &any(i_res2(3,:) .ne. 31.5) .OR. &
                &any(i_res2(4,:) .ne. 42.0) .OR. &
                &any(i_res2(5,:) .ne. 52.5) .OR. &
                &any(i_res5(1,:) .ne. (/2.0d0,3.0d0,4.0d0,5.0d0,6.0d0/)) .OR. &
                &any(i_res5(2,:) .ne. (/2.0d0,3.0d0,4.0d0,5.0d0,6.0d0/)) .OR. &
                &any(i_res5(3,:) .ne. (/2.0d0,3.0d0,4.0d0,5.0d0,6.0d0/)) .OR. &
                &any(i_res5(4,:) .ne. (/2.0d0,3.0d0,4.0d0,5.0d0,6.0d0/)) .OR. &
                &any(i_res5(5,:) .ne. (/2.0d0,3.0d0,4.0d0,5.0d0,6.0d0/)) ) then
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
            do_test4 = .true.
          end function
      end submodule subMod_leftChild_leftChild

      submodule (baseMod:subMod_leftChild) subMod_leftChild_rightChild
        contains
          module procedure do_test5
    
            i_res3 = 0.0d0
            i_res4 = 0.0d0
            i_arr3 = (/10.5,20.5,30.5,40.5,50.5,11.5,22.5,33.5,44.5,55.5/)
    
            do concurrent (i = 1:10, lvar .eqv. .TRUE.)
              i_res3(i) = sqrt(i_arr4(i))
              do concurrent (j = 1:10, mod(100.0d0,i_arr3(j)) > 0.154d2)
                i_res4(j) = sqrt(i_arr4(j))
              end do
            end do

            if ( any(i_res3 .ne. (/2.0d0,3.0d0,4.0d0,5.0d0,6.0d0,7.0d0,8.0d0,9.0d0,10.0d0,11.0d0/)) .OR. &
                &any(i_res4 .ne. (/0.0d0,0.3d1,0.0d0,0.5d1,0.6d1,0.0d0,0.0d0,0.9d1,0.0d0,1.1d1/)) ) then
              print *, "2-level nested do concurrent with mask produced incorrect results"
              print *, "i_res3: ", i_res3
              print *, "i_res4: ", i_res4
              error stop 8
            end if
            do_test5 = 1
          end procedure
      end submodule subMod_leftChild_rightChild

      submodule (baseMod:subMod_leftChild) subMod_rightChild_leftChild
        contains
          module real function do_test6()
            do_test6 = do_test6_helper()
          end function
  
          real function do_test6_helper()
            i_res2 = 0.0
            i_res5 = 0.0d0
    
            do concurrent (i = 1:5, j = 1:5, abs(-1*i_arr(i)) > 12.0 .AND. abs(-1*i_arr(j)) < 50.0)
              i_res2(i,j) = mod(100.5_8,real(i*j,8))
              do concurrent (k = 1:5, l = 1:5, abs(-1*i_arr(k)) > 26.5 .AND. abs(-1*i_arr(l)) < 40.0)
                i_res5(k,l) = sqrt(i_arr4(k))
              end do
            end do

            if ( any(i_res2(1,:) .ne. (/0.0, 0.0, 0.0, 0.0, 0.0/)) .OR. &
                &any(i_res2(2,:) .ne. (/0.5, 0.5, 4.5, 4.5, 0.0/)) .OR. &
                &any(i_res2(3,:) .ne. (/1.5, 4.5, 1.5, 4.5, 0.0/)) .OR. &
                &any(i_res2(4,:) .ne. (/0.5, 4.5, 4.5, 4.5, 0.0/)) .OR. &
                &any(i_res2(5,:) .ne. (/0.5, 0.5, 10.5, 0.5, 0.0/)) .OR. &
                &any(i_res5(1,:) .ne. 0.0d0) .OR. &
                &any(i_res5(2,:) .ne. 0.0d0) .OR. &
                &any(i_res5(3,:) .ne. (/4.0d0,4.0d0,4.0d0,0.0d0,0.0d0/)) .OR. &
                &any(i_res5(4,:) .ne. (/5.0d0,5.0d0,5.0d0,0.0d0,0.0d0/)) .OR. &
                &any(i_res5(5,:) .ne. (/6.0d0,6.0d0,6.0d0,0.0d0,0.0d0/)) ) then
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
            do_test6_helper = 1.0
          end function
      end submodule subMod_rightChild_leftChild

      submodule (baseMod:subMod_leftChild) subMod_rightChild_rightChild
        contains
          module procedure do_test7
            do_test7 = do_test7_helper()
          end procedure
 
          complex function do_test7_helper()
            i_res3 = 0.0d0
            i_res4 = 0.0d0
            i_res2 = 0.0
            i_arr3 = (/10.0,20.0,30.0,40.0,50.0,60.0,70.0,80.0,90.0,100.0/)
    
            do concurrent (i = 1:m*2, lvar .eqv. .TRUE.)
              i_res3(i) = sqrt(i_arr4(i)) 
              do concurrent (j = 100:1000:100, abs(-1*i_arr3(j/100)) >= 30.0 .AND. abs(-1*i_arr3(j/100)) <= 80.0)
                i_res4(j/100) = sqrt(i_arr4(j/100))
                do concurrent (k = 1:5, l = 1:5)
                  i_res2(k,l) = mod(100.5_8,real(k*l,8))
                end do
              end do
            end do

            if ( any(i_res3 .ne. (/2.0d0,3.0d0,4.0d0,5.0d0,6.0d0,7.0d0,8.0d0,9.0d0,10.0d0,11.0d0/)) ) then
              print *, "3-level nested do concurrent with multiple indices and masks produced incorrect results"
              print *, "failure in first, outer-most loop"
              print *, "i_res3: ", i_res3
              error stop 10
            end if

            if ( any(i_res4 .ne. (/0.0d0,0.0d0,4.0d0,5.0d0,6.0d0,7.0d0,8.0d0,9.0d0,0.0d0,0.0d0/)) ) then
              print *, "3-level nested do concurrent with multiple indices and masks produced incorrect results"
              print *, "failure in second, inner loop"
              print *, "i_res4: ", i_res4
              error stop 11
            end if

            if ( any(i_res2(1,:) .ne. (/0.5, 0.5, 1.5, 0.5, 0.5/)) .OR. &
                &any(i_res2(2,:) .ne. (/0.5, 0.5, 4.5, 4.5, 0.5/)) .OR. &
                &any(i_res2(3,:) .ne. (/1.5, 4.5, 1.5, 4.5, 10.5/)) .OR. &
                &any(i_res2(4,:) .ne. (/0.5, 4.5, 4.5, 4.5, 0.5/)) .OR. &
                &any(i_res2(5,:) .ne. (/0.5, 0.5, 10.5, 0.5, 0.5/)) ) then
              print *, "3-level nested do concurrent with multiple indices and masks produced incorrect results"
              print *, "failure in third, inner-most loop"
              print *, "i_res2(1,:): ", i_res2(1,:)
              print *, "i_res2(2,:): ", i_res2(2,:)
              print *, "i_res2(3,:): ", i_res2(3,:)
              print *, "i_res2(4,:): ", i_res2(4,:)
              print *, "i_res2(5,:): ", i_res2(5,:)
              error stop 12
            end if
            do_test7_helper = (1.0, 0.0)
          end function
      end submodule subMod_rightChild_rightChild

program main
use baseMod
  logical l1
  integer i1
  real r1
  complex c1

  call do_test0()
  call do_test1()
  call do_test2()
  call do_test3()
  l1 = do_test4()
  i1 = do_test5()
  r1 = do_test6()
  c1 = do_test7()

end
