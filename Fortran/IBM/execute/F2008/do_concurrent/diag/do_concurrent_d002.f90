!*******************************************************************************
!*
!============================================================================
!*  XL Fortran Test Case                                IBM INTERNAL USE ONLY
!*
!============================================================================
!*
!*  TEST CASE NAME             : do_concurrent_d002.f
!*
!*  PROGRAMMER                 : Bernard Kan
!*  DATE                       : 2015-03-20
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED :
!*  ADAPTED FROM               :
!*
!*  DESCRIPTION                : 
!*    - index-name is not an integer
!*      - single index
!*      - multiple index
!*
!=============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890
      type dtype
         character a
      end type dtype

      ! declarations
      integer i

      real r1, r2
      real*4 r4_1, r4_2
      real*8 r8_1, r8_2
      real*16 r16_1, r16_2

      complex c1, c2
      complex*8 c8_1, c8_2
      complex*16 c16_1, c16_2
      complex*32 c32_1, c32_2

      logical l1, l2
      logical*1 l1_1, l1_2
      logical*2 l2_1, l2_2
      logical*4 l4_1, l4_2
      logical*8 l8_1, l8_2

      type (dtype) dt

      ! 1st arg wrong
      ! real, int
      DO CONCURRENT (r1 = 0:1, i = 0:1)
      END DO

      DO CONCURRENT (r1_1 = 0:1, i = 0:1) 
      END DO

      DO CONCURRENT (r2_1 = 0:1, i = 0:1 )
      END DO

      DO CONCURRENT (r8_1 = 0:1, i = 0:1)
      END DO

      DO CONCURRENT (r16_1 = 0:1, i = 0:1)
      END DO

      ! integer, complex
      DO CONCURRENT (c1 = 0:1, i = 0:1)
      END DO

      DO CONCURRENT (c8_1 = 0:1, i = 0:1)
      END DO

      DO CONCURRENT (c16_1 = 0:1, i = 0:1)
      END DO

      DO CONCURRENT (c32_1 = 0:1, i = 0:1)
      END DO

      ! logical, integer
      DO CONCURRENT (l1 = 0:1, i = 0:1)
      END DO

      DO CONCURRENT (l1_1 = 0:1, i = 0:1)
      END DO

      DO CONCURRENT (l2_1 = 0:1, i = 0:1)
      END DO

      DO CONCURRENT (l4_1 = 0:1, i = 0:1)
      END DO

      DO CONCURRENT (l8_1 = 0:1, i = 0:1)
      END DO

      ! derived type, integer
      DO CONCURRENT (i = 0:1, dt = 0:1)
      END DO


      ! 2nd arg wrong
      ! integer, real*
      DO CONCURRENT (i = 0:1, r2 = 0:1)
      END DO

      DO CONCURRENT (i = 0:1, r4_2 = 0:1)
      END DO

      DO CONCURRENT (i = 0:1, r8_2 = 0:1)
      END DO

      DO CONCURRENT (i = 0:1, r16_2 = 0:1)
      END DO

      ! integer, complex
      DO CONCURRENT (i = 0:1, c2 = 0:1)
      END DO

      DO CONCURRENT (i = 0:1, c8_2 = 0:1)
      END DO

      DO CONCURRENT (i = 0:1, c16_2 = 0:1)
      END DO

      DO CONCURRENT (i = 0:1, c32_2 = 0:1)
      END DO

      ! integer, logical
      DO CONCURRENT (i = 0:1, l2 = 0:1)
      END DO

      DO CONCURRENT (i = 0:1, l1_2 = 0:1)
      END DO

      DO CONCURRENT (i = 0:1, l2_2 = 0:1)
      END DO

      DO CONCURRENT (i = 0:1, l4_2 = 0:1)
      END DO

      DO CONCURRENT (i = 0:1, l8_2 = 0:1)
      END DO

      ! integer, derived type
      DO CONCURRENT (i = 0:1, dt = 0:1)
      END DO

      ! random combinations
      ! 1 arg
      DO CONCURRENT (l8_2 = 0:1)
      END DO

      DO CONCURRENT (c8_2 = 0:1)
      END DO

      DO CONCURRENT (r8_2 = 0:1)
      END DO

      DO CONCURRENT (dt = 0:1)
      END DO

      ! 2 arg
      DO CONCURRENT (r1 = 0:1, l2 = 0:1)
      END DO

      DO CONCURRENT (r4_1 = 0:1, l1_2 = 0:1)
      END DO

      DO CONCURRENT (r8_1 = 0:1, l2_2 = 0:1)
      END DO

      DO CONCURRENT (r16_1 = 0:1, l4_2 = 0:1)
      END DO

      DO CONCURRENT (c32_1 = 0:1, l8_2 = 0:1)
      END DO

      DO CONCURRENT (c32_1 = 0:1, c8_2 = 0:1)
      END DO

      DO CONCURRENT (dt = 0:1, c2 = 0:1)
      END DO

      DO CONCURRENT (l4_1 = 0:1, c8_2 = 0:1)
      END DO

      DO CONCURRENT (c = 0:1, c16_2 = 0:1)
      END DO

      DO CONCURRENT (r = 0:1, c32_2 = 0:1)
      END DO

end
