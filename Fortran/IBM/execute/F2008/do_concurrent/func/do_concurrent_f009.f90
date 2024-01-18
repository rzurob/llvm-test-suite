!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : F2008/do_concurrent/func/do_concurrent_f009.f
!*
!*  DATE                       : 2015-06-08
!*
!*  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT (F2008 extension)
!*
!*  DESCRIPTION                : - DO CONCURRENT loops with variables of
!*                                 user-defined types declared without the
!*                                 ALLOCATABLE or POINTER attribute with
!*                                 nonpointer default-initialized subcomponents
!*                               - User-defined types containing reals, logicals,
!*                                 characters, complex and doubles
!*                               - scalar-mask-expr contains logicals and
!*                                 user-defined data types
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
          complex(8) :: comp8(5,5) = (1.5d0,4.6d0)
          complex(16) :: comp16(3,3,3) = (2.5q0,3.3q0)
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

        type (realType) :: dtRealType
        type (complexType) :: dtComplexType
        type (logicalType) :: dtLogicalType
        type (mixType) :: dtMixType

        integer*1 :: int1 = 0
        integer*2 :: int2 = 0, x, y, z
        integer*4 :: int4 = 0, int4_2 = 0
        integer*8 :: int8 = 0
        real*8 :: real8_result(10)
        complex(16) :: comp16_result(3,3,3)

        if ( .false. ) then
        else
          do concurrent (int1 = 5:25:5, int2 = 1:5, int4 = 5:5*5:4)
            select case(int1)
              case default
                dtRealType%real4 = 111.5e0
              case (10)
                if (int4 == 5) then
                  dtRealType%real8(9) = 245.6d0
                else if (int4 == 13) then
                  dtRealType%real8(10) = 823.44d0
                end if
            end select
          end do
        end if

        if ( .not. precision_r4(dtRealType%real4,111.5e0) ) then
          print *, "select statement in do concurrent loop in else block produces incorrect results"
          print *, "dtRealType%real4: ", dtRealType%real4
          error stop 2
        end if

        real8_result = (/25.4d0,25.4d0,25.4d0,25.4d0,25.4d0,25.4d0,25.4d0,25.4d0,245.6d0,823.44d0/)
        do x = 1,10
          if ( .not. precision_r8(dtRealType%real8(x),real8_result(x)) ) then
            print *, "select statement in do concurrent loop in else block produces incorrect results"
            print *, "x: ", x
            print *, "dtRealType%real8: ", dtRealType%real8
            error stop 2
          end if
        end do

        dtLogicalType%log8(3) = .false.
        do concurrent (int2 = 1:5, int4 = 1:5, dtLogicalType%log8(int2) .neqv. .true.)
          dtComplexType%comp8(int2, int4) = (3.0d0, 5.0d0)
        end do

        if ( .not. precision_x8(dtComplexType%comp8(3,:),(3.0d0, 5.0d0)) ) then
          print *, "do concurrent with DTP mask produces incorrect result"
          print *, "dtComplexType%comp8(3,:): ", dtComplexType%comp8(3,:)
          error stop 3
        end if

        dtMixType%char4(2) = "fail"
        dtRealType%real8 = 65.5d0
        do concurrent (int1 = 1:5, dtLogicalType%log1 .eqv. .true.)
          if (int1 == 3) then
            dtMixType%doub = 15.5d1
          end if
          do concurrent (int4 = 100:300:100, (dtLogicalType%log2 .eqv. .false.) .and. (dtMixType%char4(int4/100) .eq. "pass"))
            dtRealType%real8(int4/100) = real(int4,8)*0.5d0
            do concurrent (int2 = 1:3, int4_2 = 3:5, int8 = 100:300:100, dtLogicalType%log4 .eqv. .false.)
              dtComplexType%comp16(int2,int4_2-2,int8/100) = (2.0q0,5.0q0)*(int2,1.0q0)
            end do
          end do
        end do

        if ( .not. precision_r8(dtMixType%doub,15.5d1) ) then
          print *, "3-level nested do concurrent with multiple indices and DTP masks produced incorrect results"
          print *, "failure in first, outer-most loop"
          print *, "dtMixType%doub: ", dtMixType%doub
          error stop 4
        end if

        real8_result = (/50.0d0,65.5d0,150.0d0,65.5d0,65.5d0,65.5d0,65.5d0,65.5d0,65.5d0,65.5d0/)
        do x = 1,10
          if ( .not. precision_r8(dtRealType%real8(x),real8_result(x)) ) then
            print *, "3-level nested do concurrent with multiple indices and DTP masks produced incorrect results"
            print *, "failure in second, inner loop"
            print *, "x: ", x
            print *, "dtRealType%real8: ", dtRealType%real8
            error stop 5
          end if
        end do

        comp16_result(1,:,:) = (-3.0q0,7.0q0)
        comp16_result(2,:,:) = (-1.0q0,12.0q0)
        comp16_result(3,:,:) = (1.0q0,17.0q0)
        do x = 1,3
          do y = 1,3
            do z = 1,3
              if ( .not. precision_x6(dtComplexType%comp16(x,y,z),comp16_result(x,y,z)) ) then
                print *, "3-level nested do concurrent with multiple indices and DTP masks produced incorrect results"
                print *, "failure in third, inner-most loop"
                print *, "x: ", x
                print *, "y: ", y
                print *, "z: ", z
                print *, "dtComplexType%comp16: ", dtComplexType%comp16
                error stop 6
              end if
            end do
          end do
        end do

      end program
