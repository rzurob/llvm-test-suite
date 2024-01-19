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
!*  DESCRIPTION                : Test: ASSOCIATE with expression and with
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

      program fxass101
      implicit none

      real, dimension(2) :: arr_r, ar_r
      real*4, dimension(3) :: arr_r1, ar_r1
      real*8, dimension(4) :: arr_r2, ar_r2
      real*16, dimension(5) :: arr_r4, ar_r4

      real, dimension(4,2) :: arr_r42
      real*4, dimension(3,2) :: arr_r32
      real*8, dimension(2,2) :: arr_r22
      real*16, dimension(5,2) :: arr_r52
      integer i,j

      logical precision_r4, precision_r8, precision_r6

      arr_r = (/ 1.0,2.0 /)

      arr_r1 = (/ 1.0,2.0,3.0 /)

      arr_r2 = (/ 1.0,2.0,3.0,4.0 /)

      arr_r4 = (/ 1.0,2.0,3.0,4.0,5.0 /)

      arr_r42 = reshape( (/ 1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0 /), (/ 4,2 /) )
      arr_r32 = reshape( (/ 1.0,2.0,3.0,4.0,5.0,6.0 /), (/ 3,2 /) )
      arr_r22 = reshape( (/ 1.0,2.0,3.0,4.0 /), (/ 2,2 /) )
      arr_r52 = reshape( (/ 1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0 /), (/ 5,2 /) )

      ar_r = arr_r * 10
      ar_r1(1) = arr_r1(1) + 2
      ar_r1(2) = arr_r1(2) / 2
      ar_r1(1) = arr_r1(1) - 2
      ar_r2 = 0.0
      ar_r4 = (/ 3.0,6.0,-1.0,2.0,5.0 /)

      associate ( arr1 => arr_r(1) * 10 )
           if (.not. precision_r4(arr1,ar_r(1))) then
           error stop 1
           endif
      end associate

      associate ( arr2 => arr_r(2) * 10 )
           if (.not. precision_r4(arr2,(arr_r(2) * 10))) then
           error stop 2
           endif
      end associate

      associate ( arr3 => arr_r1(1) + 2 )
           if (.not. precision_r4(arr3,(arr_r1(1) + 2 ))) then
           error stop 3
           endif
      end associate

      associate ( arr4 => arr_r1(2) / 2 )
           if (.not. precision_r4(arr4,ar_r1(2))) then
           error stop 4
           endif
      end associate

      associate ( arr5 => arr_r1(3) - 2 )
           if (.not. precision_r4(arr5,(arr_r1(3) - 2 ))) then
           error stop 5
           endif
      end associate

      associate ( arr6 => arr_r2(1) * 0 )
           if (.not. precision_r8(arr6,ar_r2(1))) then
           error stop 6
           endif
      end associate

      associate ( arr7 => arr_r2(2) - 2 )
           if (.not. precision_r8(arr7,ar_r2(2))) then
           error stop 7
           endif
      end associate

      associate ( arr8 => (arr_r2(3) / 3) - 1 )
           if (.not. precision_r8(arr8,ar_r2(3))) then
           error stop 8
           endif
      end associate

      associate ( arr9 => arr_r2(4) * 0 )
           if (.not. precision_r8(arr9,ar_r2(4))) then
           error stop 9
           endif
      end associate

      associate ( arr10 => arr_r4(1) + arr_r4(2) )
           if (.not. precision_r6(arr10,ar_r4(1))) then
           error stop 10
           endif
      end associate

      associate ( arr11 => arr_r4(2) * arr_r4(3) )
           if (.not. precision_r6(arr11,ar_r4(2))) then
           error stop 11
           endif
      end associate

      associate ( arr12 => arr_r4(3) -  arr_r4(4) )
           if (.not. precision_r6(arr12,ar_r4(3))) then
           error stop 12
           endif
      end associate

      associate ( arr13 => arr_r4(4) / arr_r4(2) )
           if (.not. precision_r6(arr13,ar_r4(4))) then
           error stop 13
           endif
      end associate

      associate ( arr14 => arr_r4(5) * arr_r4(1) )
           if (.not. precision_r6(arr14,ar_r4(5))) then
           error stop 14
           endif
      end associate

      do i = 1,4
           do j = 1,2
      associate ( arr21 => arr_r42(i,j) + 2 )
           if (.not. precision_r4(arr21,(arr_r42(i,j) + 2))) error stop 21
      end associate
           enddo
      enddo

      do i = 1,3
           do j = 1,2
      associate ( arr22 => arr_r32(i,j) * 2 )
           if (.not. precision_r4(arr22,(arr_r32(i,j) * 2))) error stop 22
      end associate
           enddo
      enddo

      do i = 1,2
           do j = 1,2
      associate ( arr23 => arr_r22(i,j) - 2 )
           if (.not. precision_r8(arr23,(arr_r22(i,j) - 2))) error stop 23
      end associate
           enddo
      enddo

      do i = 1,5
           do j = 1,2
      associate ( arr24 => arr_r52(i,j) + 2 )
           if (.not. precision_r6(arr24,(arr_r52(i,j) + 2))) error stop 24
      end associate
           enddo
      enddo

      end
