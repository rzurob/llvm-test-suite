!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE,dimension,real
!*  TARGET(S)                  :
!*  NUMBER OF TESTS            : 1
!*  STATUS                     : done
!*
!*  STRUCTURE                  : Main program
!*  EXECUTABLE                 : Yes
!*
!*  INPUTS                     : None
!*  OUTPUTS                    : None
!*
!*  SETUP REQUIREMENTS         : N/A
!*  DEPENDENCIES               : External routine ZZRC
!*  REQUIRED COMPILER OPTIONS  : None
!*
!*  NORMAL COMPLETION          : Return code = 0
!*  ABNORMAL COMPLETION        : Return code ^= 0
!*
!*  RUN TIME ESTIMATE          : <60 SECS
!*
!*  CONDITIONS TESTED          : Listed below.
!*
!*  DESCRIPTION                : Test: ASSOCIATE with function and with
!*                                     single and double dimention array
!*                                     with using reshape and do loop
!*                                     with real, real*4, real*8, real*16
!*                                     data types.
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*                    -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      program fxass106
      implicit none

      real, dimension(4,2) :: arr_r42, ar_r42
      real*4, dimension(3,2) :: arr_r32, ar_r32
      real*8, dimension(2,2) :: arr_r22, ar_r22
      real*16, dimension(5,2) :: arr_r52, ar_r52
      integer i,j

      logical :: precision_r4, precision_r8, precision_r6

      arr_r42 = reshape( (/ 1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0 /), (/ 4,2 /) )
      arr_r32 = reshape( (/ 1.0,2.0,3.0,4.0,5.0,6.0 /), (/ 3,2 /) )
      arr_r22 = reshape( (/ 1.0,2.0,3.0,4.0 /), (/ 2,2 /) )
      arr_r52 = reshape( (/ 1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0 /), (/ 5,2 /) )

      do i = 1,4
           do j = 1,2
              ar_r42(i,j) = arr_r42(i,j) + 2
           enddo
      enddo

      do i = 1,3
           do j = 1,2
              ar_r32(i,j) = arr_r32(i,j) * 2
           enddo
      enddo
      do i = 1,2
           do j = 1,2
              ar_r22(i,j) = arr_r22(i,j) - 2
           enddo
      enddo

      do i = 1,5
           do j = 1,2
              ar_r52(i,j) = arr_r52(i,j) + 2
           enddo
      enddo

      associate ( arr21 => rel_fun(arr_r42) )
           if (.not.precision_r4(arr21,(rel_fun(arr_r42)))) then
           error stop 21
           endif
      end associate

      associate ( arr22 => rel1_fun(arr_r32) )
           if (.not.precision_r4(arr22,(rel1_fun(arr_r32)))) then
           error stop 22
           endif
      end associate

      associate ( arr23 => rel2_fun(arr_r22) )
           if (.not.precision_r8(arr23,(rel2_fun(arr_r22)))) then
           error stop 23
           endif
      end associate

      associate ( arr24 => rel4_fun(arr_r52) )
           if (.not.precision_r6(arr24,(rel4_fun(arr_r52)))) then
           error stop 24
           endif
      end associate

      contains

      real*4 function rel_fun(arr_r42)
      real*4, dimension(3,2) :: arr_r42
      rel_fun = arr_r42(1,1)
      end function

      real*4 function rel1_fun(arr_r32)
      real*4, dimension(3,2) :: arr_r32
      rel1_fun = arr_r32(1,2)
      end function

      real*8 function rel2_fun(arr_r22)
      real*8, dimension(2,2) :: arr_r22
      rel2_fun = arr_r22(2,2)
      end function

      real*16 function rel4_fun(arr_r52)
      real*16, dimension(5,2) :: arr_r52
      rel4_fun = arr_r52(3,2)
      end function

      end

