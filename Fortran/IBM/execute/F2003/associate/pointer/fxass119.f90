!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DESCRIPTION                : ASSOCIATE with pointer and derived types
!*  KEYWORD(S)                 : ASSOCIATE,derived type
!*  target(S)                  :
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
!*  DEPendENCIES               : External routine ZZRC
!*  REQUIRED COMPILER OPTIONS  : None
!*
!*  NORMAL COMPLETION          : Return code = 0
!*  ABNORMAL COMPLETION        : Return code ^= 0
!*
!*  RUN TIME ESTIMATE          : <60 SECS
!*
!*  CONDITIONS TESTED          : Listed below.
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

      program fxass119
      implicit none

      integer n,i
      type der_point
         integer i1
         integer arri1(3)
         integer arri3(-2:2)
         integer arri2(3,3)
      end type der_point

      integer, target  :: ptr1(3),ptr2(-2:2)
      integer, pointer :: p1(:), p2(:)

      type (der_point),target ::  dt
      type (der_point),pointer :: p3

      p1 => ptr1
      p2 => ptr2
      p3 => dt

      p1 = (/ 1,2,3 /)
      p2 = (/ -2,-1,0,1,2 /)

      p3%i1 = 1
      p3%arri1 = (/ 1,2,3 /)
      p3%arri3 = (/ -2,-1,0,1,2 /)
      p3%arri2 = reshape( (/ 1,2,3,4,5,6,7,8,9 /), (/ 3,3 /) )

      associate ( arg => ptr1 )
      if( arg(1) .ne. p1(1) ) error stop 1
      if( arg(2) .ne. p1(2) ) error stop 11
      if( arg(3) .ne. p1(3) ) error stop 21
      end associate

      do i= lbound(ptr2, 1), ubound(ptr2, 1)
        associate ( arg1 => ptr2(i) )
          if( arg1 .ne. p2(i) )then
            error stop 2
          endif
        end associate
      enddo

      associate ( arg2 => dt )
      if( arg2%i1 .ne. 1 )then
        error stop 3
      endif
      if( arg2%arri1(1) .ne. 1 )then
        error stop 4
      endif
      if( arg2%arri1(2) .ne. 2 )then
        error stop 5
      endif
      if( arg2%arri1(3) .ne. 3 )then
        error stop 6
      endif

      if( arg2%arri3(1) .ne. p3%arri3(1) ) error stop 7
      if( arg2%arri3(2) .ne. p3%arri3(2) ) error stop 8
      if( arg2%arri3(-1) .ne. p3%arri3(-1) ) error stop 9
      if( arg2%arri3(-2) .ne. p3%arri3(-2) ) error stop 10
      if( arg2%arri3(0) .ne. p3%arri3(0) ) error stop 11

      if( arg2%arri2(1,1) .ne. p3%arri2(1,1) ) error stop 12
      if( arg2%arri2(1,2) .ne. p3%arri2(1,2) ) error stop 13
      if( arg2%arri2(1,3) .ne. p3%arri2(1,3) ) error stop 14
      if( arg2%arri2(2,1) .ne. p3%arri2(2,1) ) error stop 15
      if( arg2%arri2(2,2) .ne. p3%arri2(2,2) ) error stop 16
      if( arg2%arri2(2,3) .ne. p3%arri2(2,3) ) error stop 17
      if( arg2%arri2(3,1) .ne. p3%arri2(3,1) ) error stop 18
      if( arg2%arri2(3,2) .ne. p3%arri2(3,2) ) error stop 19
      if( arg2%arri2(3,3) .ne. p3%arri2(3,3) ) error stop 20

      end associate

      end

