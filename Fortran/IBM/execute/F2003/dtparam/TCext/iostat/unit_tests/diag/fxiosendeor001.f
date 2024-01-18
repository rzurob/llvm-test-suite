! GB DTP extension using:
! ftcx_dtp /tstdev/F2003/iostat/unit_tests/diag/fxiosendeor001.f
! opt variations: -qck

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: fxiosendeor001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 17, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end and is_iostat_eor intrinsics
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qdebug=intmsg
!*
!*  DESCRIPTION                : This diagnostic test, makes sure that non-integer
!*                               arguments to the intrinsics are flagged.
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      integer*1 :: int1 = -1
      integer*2 :: int2 = -1
      integer*4 :: int3 = -1
      integer*8 :: int4 = -1
      integer*1 :: int1arr(3) = (/1,2,3/)
      integer*2 :: int2arr(3) = (/1,2,3/)
      integer*4 :: int3arr(3) = (/1,2,3/)
      integer*8 :: int4arr(3) = (/1,2,3/)

      logical*1 :: log1 = .true.
      logical*2 :: log2 = .true.
      logical*4 :: log3 = .true.
      logical*8 :: log4 = .true.
      logical*1 :: log1arr(3) = (/.true., .false., .true./)
      logical*2 :: log2arr(3) = (/.true., .false., .true./)
      logical*4 :: log3arr(3) = (/.true., .false., .true./)
      logical*8 :: log4arr(3) = (/.true., .false., .true./)

      real*4    :: rel1 = 1.2
      real*8    :: rel2 = 1.2
      real*4    :: rel1arr(3) = (/ 1.0, 2.1, 3.2 /)
      real*8    :: rel2arr(3) = (/ 1.0, 2.1, 3.2 /)

      complex*8   :: cmplx1 = (1,-2)
      complex*16  :: cmplx2 = (1,-2)
      complex*8   :: cmplx1arr(3) = (/ (1,-2), (-1,2), (4,4) /)
      complex*16  :: cmplx2arr(3) = (/ (1,-2), (-1,2), (4,4) /)

      character*1 :: char1 = 'x'
      character*2 :: char2 = 'x'
      character*4 :: char3 = 'x'
      character*8 :: char4 = 'x'
      character*1 :: char1arr(3) = (/ 'x', 'y', 'z' /)
      character*2 :: char2arr(3) = (/ 'x', 'y', 'z' /)
      character*4 :: char3arr(3) = (/ 'x', 'y', 'z' /)
      character*8 :: char4arr(3) = (/ 'x', 'y', 'z' /)

      double precision :: dbl1 = 3.33
      double precision :: dbl1arr(3) = (/ 3.33, 4.44, 5.55 /)

      logical   :: ignore
      logical   :: igArr(3)

      type a_type(n1,d1)    ! (10,4)
         integer, kind :: d1
         integer, len  :: n1
         character(n1) :: cc
         integer(d1)   :: aa
      end type a_type

      type(a_type(10,4)) :: avar
      type(a_type(10,4)), dimension(3) :: arravar

  ! IS_IOSTAT_END:

      ! these should be ok
      ignore = is_iostat_end(int1)
      ignore = is_iostat_end(int2)
      ignore = is_iostat_end(int3)
      ignore = is_iostat_end(int4)
      igArr = is_iostat_end(int1arr)
      igArr = is_iostat_end(int2arr)
      igArr = is_iostat_end(int3arr)
      igArr = is_iostat_end(int4arr)

      ! these should generate error
      ignore = is_iostat_end(log1)
      ignore = is_iostat_end(log2)
      ignore = is_iostat_end(log3)
      ignore = is_iostat_end(log4)
      igArr = is_iostat_end(log1arr)
      igArr = is_iostat_end(log2arr)
      igArr = is_iostat_end(log3arr)
      igArr = is_iostat_end(log4arr)

      ignore = is_iostat_end(rel1)
      ignore = is_iostat_end(rel2)
      igArr = is_iostat_end(rel1arr)
      igArr = is_iostat_end(rel2arr)
      ignore = is_iostat_end(dbl1)
      igArr = is_iostat_end(dbl1arr)

      ignore = is_iostat_end(cmplx1)
      ignore = is_iostat_end(cmplx2)
      igArr = is_iostat_end(cmplx1arr)
      igArr = is_iostat_end(cmplx2arr)

      ignore = is_iostat_end(char1)
      ignore = is_iostat_end(char2)
      ignore = is_iostat_end(char3)
      ignore = is_iostat_end(char4)
      igArr = is_iostat_end(char1arr)
      igArr = is_iostat_end(char2arr)
      igArr = is_iostat_end(char3arr)
      igArr = is_iostat_end(char4arr)

      ignore = is_iostat_end(avar)
      igArr = is_iostat_end(arravar)

  ! IS_IOSTAT_EOR:

      ! these should be ok
      ignore = is_iostat_eor(int1)
      ignore = is_iostat_eor(int2)
      ignore = is_iostat_eor(int3)
      ignore = is_iostat_eor(int4)
      igArr = is_iostat_eor(int1arr)
      igArr = is_iostat_eor(int2arr)
      igArr = is_iostat_eor(int3arr)
      igArr = is_iostat_eor(int4arr)

      ! these should generate error
      ignore = is_iostat_eor(log1)
      ignore = is_iostat_eor(log2)
      ignore = is_iostat_eor(log3)
      ignore = is_iostat_eor(log4)
      igArr = is_iostat_eor(log1arr)
      igArr = is_iostat_eor(log2arr)
      igArr = is_iostat_eor(log3arr)
      igArr = is_iostat_eor(log4arr)

      ignore = is_iostat_eor(rel1)
      ignore = is_iostat_eor(rel2)
      igArr = is_iostat_eor(rel1arr)
      igArr = is_iostat_eor(rel2arr)
      ignore = is_iostat_eor(dbl1)
      igArr = is_iostat_eor(dbl1arr)

      ignore = is_iostat_eor(cmplx1)
      ignore = is_iostat_eor(cmplx2)
      igArr = is_iostat_eor(cmplx1arr)
      igArr = is_iostat_eor(cmplx2arr)

      ignore = is_iostat_eor(char1)
      ignore = is_iostat_eor(char2)
      ignore = is_iostat_eor(char3)
      ignore = is_iostat_eor(char4)
      igArr = is_iostat_eor(char1arr)
      igArr = is_iostat_eor(char2arr)
      igArr = is_iostat_eor(char3arr)
      igArr = is_iostat_eor(char4arr)

      ignore = is_iostat_eor(avar)
      igArr = is_iostat_eor(arravar)

      end
