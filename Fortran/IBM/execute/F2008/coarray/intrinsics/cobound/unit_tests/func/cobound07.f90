!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 20, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Functionality of cobound intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qintsize=8
!*
!*  DESCRIPTION                : Testing kind type parameter of cobound
!*                               intrinsics when -qintsize is used.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      complex, save :: coarr1(10)[*]
      real, save :: coarr2(10)[666:*]

      integer :: def_int

      if (kind(def_int) .ne. 8) then
         print *, "this test case must be compiled with -qintsize=8"
         error stop 1
      end if

      if (kind(ucobound(coarr1)) .ne. 8) error stop 2
      if (kind(ucobound(coarr1, kind=4)) .ne. 4) error stop 3
      if (kind(ucobound(coarr1, 1, 4)) .ne. 4) error stop 4
      if (kind(ucobound(coarr1, kind=8)) .ne. 8) error stop 5

      call foo(lcobound(coarr2, 1, kind=8))
      call foo(lcobound(coarr2, 1))
      call bar(lcobound(coarr2, 1, kind=4))

      sync all

      contains
      subroutine foo(arg)
        integer(kind=8) :: arg
        if (arg .ne. 666_8) error stop 6
      end subroutine

      subroutine bar(arg)
        integer(kind=4) :: arg
        if (arg .ne. 666_4) error stop 7
      end subroutine


      end
