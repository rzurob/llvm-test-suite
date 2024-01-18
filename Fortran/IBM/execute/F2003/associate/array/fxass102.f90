!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE,dimension,complex
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
!*                                     with complex, complex*8, complex*16
!*                                     complex*32 and double complex data
!*                                     types.
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

      program fxass102
      implicit none

      complex, dimension(2) :: arr_c, ar_c
      complex*8, dimension(3) :: arr_c1, ar_c1
      complex*16, dimension(4) :: arr_c2, ar_c2
      complex*32, dimension(5) :: arr_c4, ar_c4
      double complex, dimension(6) :: arr_c8, ar_c8

      complex, dimension(4,2) :: arr_c42, ar_c42
      complex*8, dimension(3,2) :: arr_c32, ar_c32
      complex*16, dimension(2,2) :: arr_c22, ar_c22
      complex*32, dimension(5,2) :: arr_c52, ar_c52
      double complex, dimension(1,2) :: arr_c12, ar_c12
      integer i,j

      logical  precision_x8, precision_x6, precision_x3

      arr_c = (/ (1.0,1.0),(2.0,2.0) /)

      arr_c1 = (/ (1.0,1.0),(2.0,2.0),(3.0,3.0) /)

      arr_c2 = (/ (1.0,1.0),(2.0,2.0),(3.0,3.0),(4.0,4.0) /)

      arr_c4 = (/ (1.0,1.0),(2.0,2.0),(3.0,3.0),(4.0,4.0),(5.0,5.0) /)

      arr_c8 = (/ (1.0,1.0),(2.0,2.0),(3.0,3.0),(4.0,4.0),(5.0,5.0), &
     +(6.0,6.0) /)

      arr_c42 = reshape( (/ &
     +(1.0,1.0),(2.0,2.0),(3.0,3.0),(4.0,4.0),(5.0,5.0),(6.0,6.0), &
     +(7.0,7.0),(8.0,8.0) /), (/ 4,2 /) )
      arr_c32 = reshape( (/ &
     +1,2,3,4,5,6 /), (/ 3,2 /) )
      arr_c22 = reshape( (/ (1.0,1.0),(2.0,2.0),(3.0,3.0),(4.0,4.0) /), &
     +(/ 2,2 /) )
      arr_c52 = reshape( &
     +(/ (1.0,1.0),(2.0,2.0),(3.0,3.0),(4.0,4.0),(5.0,5.0), &
     +(6.0,6.0),(7.0,7.0),(8.0,8.0),(9.0,9.0),(10.0,10.0) /), (/ 5,2 /) )
      arr_c12 = reshape( (/ (4.0,4.0),(6.0,6.0) /), (/ 1,2 /) )

      ar_c = arr_c * 10
      ar_c1(1) = arr_c1(1) + (2.0,2.0)
      ar_c1(2) = arr_c1(2) / (2.0,2.0)
      ar_c1(1) = arr_c1(1) - (2.0,2.0)
      ar_c2 = (0.0,0.0)
      ar_c4 = (/ (3.0,3.0),(6.0,6.0),(-1.0,-1.0),(2.0,2.0),(5.0,5.0) /)
      ar_c8 = (/ (1.0,1.0),(2.0,2.0),(3.0,3.0),(4.0,4.0),(5.0,5.0), &
     +(6.0,6.0) /)

      do i = 1,4
           do j = 1,2
              ar_c42(i,j) = arr_c42(i,j) + (2.0,2.0)
           enddo
      enddo

      do i = 1,3
           do j = 1,2
              ar_c32(i,j) = arr_c32(i,j) * (2.0,2.0)
           enddo
      enddo
      do i = 1,2
           do j = 1,2
              ar_c22(i,j) = arr_c22(i,j) - (2.0,2.0)
           enddo
      enddo

      do i = 1,5
           do j = 1,2
              ar_c52(i,j) = arr_c52(i,j) + (2.0,2.0)
           enddo
      enddo

      i = 1
           do j = 1,2
              ar_c12(i,j) = arr_c12(i,j) / (2.0,2.0)
           enddo

      associate ( arr1 => arr_c(1) * 10 )
           if (.not.precision_x8(arr1,ar_c(1))) then
           error stop 1
           endif
      end associate

      associate ( arr2 => arr_c(2) * 10 )
           if (.not.precision_x8(arr2,ar_c(2))) then
           error stop 2
           endif
      end associate

      associate ( arr3 => arr_c1(1) + (2.0,2.0) )
           if (.not.precision_x8(arr3,(arr_c1(1) + (2.0,2.0)))) then
           error stop 3
           endif
      end associate

      associate ( arr4 => arr_c1(2) / (2.0,2.0) )
           if (.not.precision_x8(arr4,ar_c1(2))) then
           error stop 4
           endif
      end associate

      associate ( arr5 => arr_c1(3) - (2.0,2.0) )
           if (.not.precision_x8(arr5,(arr_c1(3) - (2.0,2.0)))) then
           error stop 5
           endif
      end associate

      associate ( arr6 => arr_c2(1) * (0.0,0.0) )
           if (.not.precision_x6(arr6,ar_c2(1))) then
           error stop 6
           endif
      end associate

      associate ( arr7 => arr_c2(2) - (2.0,2.0) )
           if (.not.precision_x6(arr7,ar_c2(2))) then
           error stop 7
           endif
      end associate

      associate ( arr8 => (arr_c2(3) / (3.0,3.0)) - (1.0,1.0) )
           if (.not.precision_x6(arr8,((arr_c2(3) / (3.0,3.0)) - (1.0,1.0)))) then
           error stop 8
           endif
      end associate

      associate ( arr9 => arr_c2(4) * (0.0,0.0) )
           if (.not.precision_x6(arr9,ar_c2(4))) then
           error stop 9
           endif
      end associate

      associate ( arr10 => arr_c4(1) + arr_c4(2) )
           if (.not.precision_x3(arr10,ar_c4(1))) then
           error stop 10
           endif
      end associate

      associate ( arr11 => arr_c4(2) * arr_c4(3) )
           if (.not.precision_x3(arr11,(arr_c4(2) * arr_c4(3)))) then
           error stop 11
           endif
      end associate

      associate ( arr12 => arr_c4(3) -  arr_c4(4) )
           if (.not.precision_x3(arr12,ar_c4(3))) then
           error stop 12
           endif
      end associate

      associate ( arr13 => arr_c4(4) / arr_c4(2) )
           if (.not.precision_x3(arr13,(arr_c4(4) / arr_c4(2)))) then
           error stop 13
           endif
      end associate

      associate ( arr14 => arr_c4(5) * arr_c4(1) )
           if (.not.precision_x3(arr14,(arr_c4(5) * arr_c4(1)))) then
           error stop 14
           endif
      end associate

      associate ( arr15 => arr_c8(5) - arr_c8(4) )
           if (.not.precision_x6(arr15,ar_c8(1))) then
           error stop 15
           endif
      end associate

      associate ( arr16 => arr_c8(6) / arr_c8(3) )
           if (.not.precision_x6(arr16,(arr_c8(6) / arr_c8(3)))) then
           error stop 16
           endif
      end associate

      associate ( arr17 => arr_c8(5) -  arr_c8(2) )
           if (.not.precision_x6(arr17,ar_c8(3))) then
           error stop 17
           endif
      end associate

      associate ( arr18 => arr_c8(4) * arr_c8(1) )
           if (.not.precision_x6(arr18,(arr_c8(4) * arr_c8(1)))) then
           error stop 18
           endif
      end associate

      associate ( arr19 => arr_c8(3) + arr_c8(2) )
           if (.not.precision_x6(arr19,ar_c8(5))) then
           error stop 19
           endif
      end associate

      associate ( arr20 => arr_c8(2) * arr_c8(3) )
           if (.not.precision_x6(arr20,(arr_c8(2) * arr_c8(3)))) then
           error stop 20
           endif
      end associate

      do i = 1,4
           do j = 1,2
      associate ( arr21 => arr_c42(i,j) + 2 )
           if (.not.precision_x8(arr21,(arr_c42(i,j) + 2))) then
           error stop 21
           endif
      end associate
           enddo
      enddo

      do i = 1,3
           do j = 1,2
      associate ( arr22 => arr_c32(i,j) * 2 )
           if (.not.precision_x8(arr22,(arr_c32(i,j) * 2))) then
           error stop 22
           endif
      end associate
           enddo
      enddo

      do i = 1,2
           do j = 1,2
      associate ( arr23 => arr_c22(i,j) - 2 )
           if (.not.precision_x6(arr23,(arr_c22(i,j) - 2))) then
           error stop 23
           endif
      end associate
           enddo
      enddo
      do i = 1,5
           do j = 1,2
      associate ( arr24 => arr_c52(i,j) + 2 )
           if (.not.precision_x3(arr24,(arr_c52(i,j) + 2))) then
           error stop 24
           endif
      end associate
           enddo
      enddo

      i = 1
           do j = 1,2
      associate ( arr25 => arr_c12(i,j) / 2 )
           if (.not.precision_x6(arr25,(arr_c12(i,j) / 2))) then
           error stop 25
           endif
      end associate
           enddo

      end
