!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE,dimension,integer
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
!*                                     with integer, integer*1, integer*2
!*                                     integer*4, integer*8 data types.
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

      program fxass100
      implicit none

      integer, dimension(2) :: arr_i, ar_i
      integer*1, dimension(3) :: arr_i1, ar_i1
      integer*2, dimension(4) :: arr_i2, ar_i2
      integer*4, dimension(5) :: arr_i4, ar_i4
      integer*8, dimension(6) :: arr_i8, ar_i8

      integer, dimension(4,2) :: arr_i42
      integer*1, dimension(3,2) :: arr_i32
      integer*2, dimension(2,2) :: arr_i22
      integer*4, dimension(5,2) :: arr_i52
      integer*8, dimension(1,2) :: arr_i12
      integer i,j

      arr_i = (/ 1,2 /)

      arr_i1 = (/ 1,2,3 /)

      arr_i2 = (/ 1,2,3,4 /)

      arr_i4 = (/ 1,2,3,4,5 /)

      arr_i8 = (/ 1,2,3,4,5,6 /)

      arr_i42 = reshape( (/ 1,2,3,4,5,6,7,8 /), (/ 4,2 /) )
      arr_i32 = reshape( (/ 1,2,3,4,5,6 /), (/ 3,2 /) )
      arr_i22 = reshape( (/ 1,2,3,4 /), (/ 2,2 /) )
      arr_i52 = reshape( (/ 1,2,3,4,5,6,7,8,9,10 /), (/ 5,2 /) )
      arr_i12 = reshape( (/ 4,6 /), (/ 1,2 /) )

      ar_i = arr_i * 10
      ar_i1(1) = arr_i1(1) + 2
      ar_i1(2) = arr_i1(2) / 2
      ar_i1(3) = arr_i1(3) - 2
      ar_i2 = 0
      ar_i4 = (/ 3,6,-1,2,5 /)
      ar_i8 = (/ 1,2,3,4,5,6 /)

      associate ( arr1 => arr_i(1) * 10 )
           if(arr1 .ne. ar_i(1))then
           error stop 1
           endif
      end associate

      associate ( arr2 => arr_i(2) * 10 )
           if(arr2 .ne. ar_i(2))then
           error stop 2
           endif
      end associate

      associate ( arr3 => arr_i1(1) + 2_1 )
           if(arr3 .ne. ar_i1(1))then
           error stop 3
           endif
      end associate

      associate ( arr4 => arr_i1(2) / 2_1 )
           if(arr4 .ne. ar_i1(2))then
           error stop 4
           endif
      end associate

      associate ( arr5 => arr_i1(3) - 2_1 )
           if(arr5 .ne. ar_i1(3))then
           error stop 5
           endif
      end associate

      associate ( arr6 => arr_i2(1) * 0_2 )
           if(arr6 .ne. ar_i2(1))then
           error stop 6
           endif
      end associate

      associate ( arr7 => arr_i2(2) - 2_2 )
           if(arr7 .ne. ar_i2(2))then
           error stop 7
           endif
      end associate

      associate ( arr8 => (arr_i2(3) / 3_2) - 1_2 )
           if(arr8 .ne. ar_i2(3))then
           error stop 8
           endif
      end associate

      associate ( arr9 => arr_i2(4) * 0_2 )
           if(arr9 .ne. ar_i2(4))then
           error stop 9
           endif
      end associate

      associate ( arr10 => arr_i4(1) + arr_i4(2) )
           if(arr10 .ne. ar_i4(1))then
           error stop 10
           endif
      end associate

      associate ( arr11 => arr_i4(2) * arr_i4(3) )
           if(arr11 .ne. ar_i4(2))then
           error stop 11
           endif
      end associate

      associate ( arr12 => arr_i4(3) -  arr_i4(4) )
           if(arr12 .ne. ar_i4(3))then
           error stop 12
           endif
      end associate

      associate ( arr13 => arr_i4(4) / arr_i4(2) )
           if(arr13 .ne. ar_i4(4))then
           error stop 13
           endif
      end associate

      associate ( arr14 => arr_i4(5) * arr_i4(1) )
           if(arr14 .ne. ar_i4(5))then
           error stop 14
           endif
      end associate

      associate ( arr15 => arr_i8(5) - arr_i8(4) )
           if(arr15 .ne. ar_i8(1))then
           error stop 15
           endif
      end associate

      associate ( arr16 => arr_i8(6) / arr_i8(3) )
           if(arr16 .ne. ar_i8(2))then
           error stop 16
           endif
      end associate

      associate ( arr17 => arr_i8(5) -  arr_i8(2) )
           if(arr17 .ne. ar_i8(3))then
           error stop 17
           endif
      end associate

      associate ( arr18 => arr_i8(4) * arr_i8(1) )
           if(arr18 .ne. ar_i8(4))then
           error stop 18
           endif
      end associate

      associate ( arr19 => arr_i8(3) + arr_i8(2) )
           if(arr19 .ne. ar_i8(5))then
           error stop 19
           endif
      end associate

      associate ( arr20 => arr_i8(2) * arr_i8(3) )
           if(arr20 .ne. ar_i8(6))then
           error stop 20
           endif
      end associate

      do i = 1,4
           do j = 1,2
      associate ( arr21 => arr_i42(i,j) + 2 )
           if(arr21 .ne. (arr_i42(i,j) + 2)) error stop 21
      end associate
           enddo
      enddo

      do i = 1,3
           do j = 1,2
      associate ( arr22 => arr_i32(i,j) * 2_1 )
           if(arr22 .ne. (arr_i32(i,j) * 2_1)) error stop 22
      end associate
           enddo
      enddo

      do i = 1,2
           do j = 1,2
      associate ( arr23 => arr_i22(i,j) - 2_2 )
           if(arr23 .ne. (arr_i22(i,j) - 2_2)) error stop 23
      end associate
           enddo
      enddo

      do i = 1,5
           do j = 1,2
      associate ( arr24 => arr_i52(i,j) + 2 )
           if(arr24 .ne. (arr_i52(i,j) + 2)) error stop 24
      end associate
           enddo
      enddo

      i = 1
           do j = 1,2
      associate ( arr25 => arr_i12(i,j) / 2 )
           if(arr25 .ne. (arr_i12(i,j) / 2)) error stop 25
      end associate
           enddo

      end
