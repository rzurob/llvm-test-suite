!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : F2008/do_concurrent/func/do_concurrent_f011.f
!*
!*  PROGRAMMER                 : Nicole Negherbon 
!*  DATE                       : 2015-07-27
!*
!*  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT (F2008 extension)
!*
!*  DESCRIPTION                : - DO CONCURRENT loops (including nested DO 
!*                                 CONCURRENT loops) with blocks inside them
!*                               - User-defined data types containing reals, 
!*                                 logicals, characters, complex and doubles
!*                               - scalar-mask-expr contains user-defined data 
!*                                 types
!*                               - DO CONCURRENT loops (including nested DO 
!*                                 CONCURRENT loops) acting on BLOCK local 
!*                                 variables
!*                               - Select statement inside DO CONCURRENT loop 
!*                                 with user-defined data types inside
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890
      module m
        type realType
          real*4 :: real4 = 50.2e0
          real*8 :: real8(10) = 25.4d0
        end type realType

        type complexType
          complex*8 :: comp8(5,5) = (1.5e0,4.6e0)
          complex*16 :: comp16(3,3,3) = (2.5d0,3.3d0)
        end type complexType

        type logicalType
          logical*1 :: log1 = .true.
          logical*2 :: log2 = .false.
          logical*4 :: log4 = .false.
          logical*8 :: log8(5) = .true.
        end type logicalType

        type mixType
          character*4 :: char4(10) = "pass"
          double precision :: doub = 45.64d2
        end type mixType
      end module

      program main
        use m

        implicit none

        logical, external :: precision_x6, precision_x8, precision_r4, precision_r8

        integer*1 :: int1, x, y, z
        integer*2 :: int2
        integer*4 :: int4
        integer*8 :: int8
        real*8 :: real8_result(10)
        complex*8 :: comp8_result(5,5)
        complex*16 :: comp16_result(3,3,3)
 
        type (realType) dtRealType
        type (complexType) dtComplexType
        type (logicalType) dtLogicalType
        type (mixType) dtMixType

        
        do concurrent (int1 = 5:25:5, int2 = 1:5, int4 = 5:5*5:4)
          block
            real*4 :: tmp1 = 111.5e0
            real*8 :: tmp2 = 245.6d0, tmp3 = 823.44d0
            select case(int1)
              case default
                dtRealType%real4 = tmp1
              case (10)
                if (int4 == 5) then
                  dtRealType%real8(9) = tmp2
                else if (int4 == 13) then
                  dtRealType%real8(10) = tmp3
                end if
            end select
          end block
        end do

        if ( .not. precision_r4(dtRealType%real4,111.5e0) ) then
          print *, "select statement inside block in do concurrent loop inside block produces incorrect results"
          print *, "dtRealType%real4: ", dtRealType%real4
          error stop 1 
        end if 

        real8_result = (/25.4d0,25.4d0,25.4d0,25.4d0,25.4d0,25.4d0,25.4d0,25.4d0,245.6d0,823.44d0/)
        do x = 1,10
          if ( .not. precision_r8(dtRealType%real8(x),real8_result(x)) ) then
            print *, "select statement inside block in do concurrent loop inside block produces incorrect results"
            print *, "x: ", x
            print *, "dtRealType%real8: ", dtRealType%real8
            error stop 1
          end if
        end do

        dtLogicalType%log8(3) = .false.
        do concurrent (int2 = 1:5, int4 = 1:5, dtLogicalType%log8(int2) .neqv. .true.)
          block
            complex*8 :: tmp = (3.0e0, 5.0e0)
            dtComplexType%comp8(int2, int4) = tmp
          end block
        end do

        comp8_result = (1.5e0,4.6e0)
        comp8_result(3,:) = (3.0e0, 5.0e0)
        do x = 1,5
          do y = 1,5
            if ( .not. precision_x8(dtComplexType%comp8(x,y),comp8_result(x,y)) ) then
              print *, "do concurrent with DTP mask and a block inside produces incorrect result"
              print *, "x: ", x
              print *, "y: ", y
              print *, "dtComplexType%comp8: ", dtComplexType%comp8
              error stop 2
            end if
          end do
        end do
      
        dtMixType%char4(2) = "fail" 
        dtRealType%real8 = 65.5d0
        do concurrent (int1 = 1:3, dtLogicalType%log1 .eqv. .true.)
          if (int1 == 3) then
            dtMixType%doub = 15.5d1
          end if
          block
            integer*2 :: int2_tmp
            integer*4 :: int4_tmp, int4_2_tmp
            do concurrent (int4_tmp = 100:300:100, (dtLogicalType%log2 .eqv. .false.) .and. (dtMixType%char4(int4_tmp/100) .eq. "pass"))
              block
                real*8 :: tmp
                tmp = real(int4_tmp,8)*0.5d0
                dtRealType%real8(int4_tmp/100) = tmp
                do concurrent (int2_tmp = 1:3, int4_2_tmp = 3:5, int8 = 100:300:100, dtLogicalType%log4 .eqv. .false.)
                  block
                    complex*16 :: tmp2 = (2.0q0,5.0q0)
                    dtComplexType%comp16(int2_tmp,int4_2_tmp-2,int8/100) = tmp2*(int2_tmp,1.0q0)
                  end block
                end do
              end block
            end do
          end block
        end do

        if ( .not. precision_r8(dtMixType%doub,15.5d1) ) then
          print *, "3-level nested do concurrent with multiple indices and masks initialized with DATA statements produced incorrect results"
          print *, "failure in first, outer-most loop"
          print *, "dtMixType%doub: ", dtMixType%doub
          error stop 3
        end if

        real8_result = (/50.0d0,65.5d0,150.0d0,65.5d0,65.5d0,65.5d0,65.5d0,65.5d0,65.5d0,65.5d0/)
        do x = 1,10
          if ( .not. precision_r8(dtRealType%real8(x),real8_result(x)) ) then
            print *, "3-level nested do concurrent with multiple indices and masks initialized with DATA statements produced incorrect results"
            print *, "failure in second, inner loop"
            print *, "x: ", x
            print *, "dtRealType%real8: ", dtRealType%real8
            error stop 4
          end if
        end do

        comp16_result(1,:,:) = (-3.0q0,7.0q0)
        comp16_result(2,:,:) = (-1.0q0,12.0q0)
        comp16_result(3,:,:) = (1.0q0,17.0q0)
        do x = 1,3
          do y = 1,3
            do z = 1,3
              if ( .not. precision_x6(dtComplexType%comp16(x,y,z),comp16_result(x,y,z)) ) then
                print *, "3-level nested do concurrent with multiple indices and masks initialized with DATA statements produced incorrect results"
                print *, "failure in third, inner-most loop"
                print *, "x: ", x
                print *, "y: ", y
                print *, "z: ", z
                print *, "dtComplexType%comp16: ", dtComplexType%comp16
                error stop 5
              end if
            end do
          end do
        end do

      end program
