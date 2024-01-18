!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2015-05-14
!*
!*  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT (F2008 extension)
!*
!*  DESCRIPTION                : - Various kinds of real, complex, double,
!*                                 logical and character arrays and scalars in
!*                                 DO CONCURRENT loops including nested DO
!*                                 CONCURRENT loops
!*                               - scalar-mask-expr contains logicals and
!*                                 character arrays
!*                               - DATA statements used to initialize values
!*                               - Select statements inside DO CONCURRENT loop
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890
      program main
        implicit none

        logical, external :: precision_x6, precision_x8, precision_r4, precision_r8

        integer :: i, j, x, y, z
        integer*1 :: int1
        integer*2 :: int2
        integer*4 :: int4, int4_2
        integer*8 :: int8
        real*4 :: real4
        real*8 :: real8(10), real8_result(10)
        complex(8) :: comp8(5,5), comp8_result(5,5)
        complex(16) :: comp16(3,3,3), comp16_result(3,3,3)
        logical*1 :: log1
        logical*2 :: log2
        logical*4 :: log4
        logical*8 :: log8(5)
        character(len=4) :: char4(10)
        double precision :: doub

        data int1 / 100 /
        data int2 / 200 /
        data int4 / 300 /
        data int4_2 / 300 /
        data int8 / 400 /
        data real4 / 50.2e0 /
        data real8 / 10*25.4d0 /
        data comp8 / 25*(1.5d0,4.6d0) /
        data comp16 / 27*(2.5q0,3.3q0) /
        data log1 / .true. /
        data log2 / .false. /
        data log4 / .false. /
        data log8 / 5*.true. /
        data char4 / 10*"pass" /
        data doub / 45.64d02 /

        do concurrent (int1 = 1:50:5, int2 = 1:200:10)
        end do

        if (int1 .ne. 100) then
          print *, "do concurrent incrementer initialized with DATA statement modified an external scope variable"
          print *, "int1: ", int1
          error stop 1
        end if

        if (int2 .ne. 200) then
          print *, "do concurrent incrementer initialized with DATA statement modified an external scope variable"
          print *, "int2: ", int2
          error stop 2
        else
          do concurrent (int1 = 5:25:5, int2 = 1:5, int4 = 5:5*5:4)
            select case(int1)
              case default
                real4 = 111.5e0
              case (10)
                if (int4 == 5) then
                  real8(9) = 245.6d0
                else if (int4 == 13) then
                  real8(10) = 823.44d0
                end if
            end select
          end do
        end if

        real8_result = (/25.4d0,25.4d0,25.4d0,25.4d0,25.4d0,25.4d0,25.4d0,25.4d0,245.6d0,823.44d0/)
        if ( .not. precision_r4(real4,111.5e0) ) then
          print *, "select statement in do concurrent loop in else block produces incorrect results"
          print *, "real4: ", real4
          error stop 3
        end if

        do x = 1,10
          if ( .not. precision_r8(real8(x),real8_result(x)) ) then
            print *, "select statement in do concurrent loop in else block produces incorrect results"
            print *, "x: ", x
            print *, "real8: ", real8
            error stop 3
          end if
        end do

        log8(3) = .false.
        do concurrent (int2 = 1:5, int4 = 1:5, log8(int2) .neqv. .true.)
          comp8(int2, int4) = (3.0d0, 5.0d0)
        end do

        comp8_result = (1.5d0,4.6d0)
        comp8_result(3,:) = (3.0d0, 5.0d0)
        do x = 1,5
          do y = 1,5
            if ( .not. precision_x8(comp8(x,y),comp8_result(x,y)) ) then
              print *, "do concurrent with mask initialized with DATA statement produces incorrect result"
              print *, "x: ", x
              print *, "y: ", y
              print *, "comp8: ", comp8
              error stop 4
            end if
          end do
        end do

        char4(2) = "fail"
        real8 = 65.5d0
        do concurrent (int1 = 1:3, log1 .eqv. .true.)
          if (int1 == 3) then
            doub = 15.5d1
          end if
          do concurrent (int4 = 100:300:100, (log2 .eqv. .false.) .and. (char4(int4/100) .eq. 'pass'))
            real8(int4/100) = real(int4,8)*0.5d0
            do concurrent (int2 = 1:3, int4_2 = 3:5, int8 = 100:300:100, log4 .eqv. .false.)
              comp16(int2,int4_2-2,int8/100) = (2.0q0,5.0q0)*(int2,1.0q0)
            end do
          end do
        end do

        if ( .not. precision_r8(doub,15.5d1) ) then
          print *, "3-level nested do concurrent with multiple indices and masks initialized with DATA statements produced incorrect results"
          print *, "failure in first, outer-most loop"
          print *, "doub: ", doub
          error stop 5
        end if

        real8_result = (/50.0d0,65.5d0,150.0d0,65.5d0,65.5d0,65.5d0,65.5d0,65.5d0,65.5d0,65.5d0/)
        do x = 1,10
          if ( .not. precision_r8(real8(x),real8_result(x)) ) then
            print *, "3-level nested do concurrent with multiple indices and masks initialized with DATA statements produced incorrect results"
            print *, "failure in second, inner loop"
            print *, "x: ", x
            print *, "real8: ", real8
            error stop 6
          end if
        end do

        comp16_result(1,:,:) = (-3.0q0,7.0q0)
        comp16_result(2,:,:) = (-1.0q0,12.0q0)
        comp16_result(3,:,:) = (1.0q0,17.0q0)
        do x = 1,3
          do y = 1,3
            do z = 1,3
              if ( .not. precision_x6(comp16(x,y,z),comp16_result(x,y,z)) ) then
                print *, "3-level nested do concurrent with multiple indices and masks initialized with DATA statements produced incorrect results"
                print *, "failure in third, inner-most loop"
                print *, "x: ", x
                print *, "y: ", y
                print *, "z: ", z
                print *, "comp16: ", comp16
                error stop 7
              end if
            end do
          end do
        end do

      end program
