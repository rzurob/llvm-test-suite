! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: asystrm.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*
!*  ===================================================================
!*
!*                             :
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  DIAGNOSES TESTED           : A unit connected for STREAM access
!*                             : is used for asynchronous I/O.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890


      subroutine compare(istart, iend, isize, a)
         integer, dimension(isize) :: a
         integer i, istart, iend, isize
         do i = istart, iend
            print*, i, a(i)
            if (a(i) <> i) error stop 5
         end do
      end subroutine compare

      Program asystrm

      integer, parameter :: isize = 10
      integer, parameter :: sect1 = (isize/2), sect2 = isize
      integer, dimension(isize), static :: a
      integer idvar, iostat, j

      open(10, status='scratch', access='stream', asynch='yes', iostat=iostat)
      if (iostat <> 0) error stop 1

      do j = 1, isize
      write(10, iostat=iostat) j
      if (iostat <> 0) error stop 2
      end do

      a = 0
      rewind(10)
      read(10) a(1:sect1)
      if (iostat <> 0) error stop 3

      read(10, id=idvar, iostat=iostat) a(sect1+1:sect2)
      if (iostat <> 0) error stop 4

      call compare(1, sect1, isize, a)

      wait(id=idvar)
      call compare(sect1+1, sect2, isize, a)

      End Program asystrm
