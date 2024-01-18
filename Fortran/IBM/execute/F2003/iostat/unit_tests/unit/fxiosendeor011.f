!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: fxiosendeor011.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : fxiosendeor011
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Aug. 19, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end and is_iostat_eor intrinsics
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : This tests the functionality of the intrinsic when used
!*                               in specification expressions.
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
 
      character( len = is_iostat_end(-2), kind = is_iostat_end(-2) ) :: &
     &           aa
      character( len = is_iostat_end(-4), kind = is_iostat_eor(-1)+1 )::&
     &           bb
      character( len = is_iostat_end(-1), kind = is_iostat_end(-1) ) :: &
     &           cc

      character( len = is_iostat_eor(-4), kind = is_iostat_eor(-4) ) :: &
     &           aaa
      character( len = is_iostat_eor(-2), kind = is_iostat_eor(-1)+1 )::&
     &           bbb
      character( len = is_iostat_eor(-1), kind = is_iostat_eor(-1)+1 )::&
     &           ccc
     
      real, dimension(is_iostat_end(-1))   :: rr1end 
      real, dimension(is_iostat_end(-2)*4) :: rr2end
      real, dimension(is_iostat_end(-4))   :: rr3end

      real, dimension(is_iostat_eor(-1))   :: rr1eor 
      real, dimension(is_iostat_eor(-2)*4) :: rr2eor
      real, dimension(is_iostat_eor(-4))   :: rr3eor
 
     
      write(*,*) "len = ", len(aa), " kind = ", kind(aa)
      write(*,*) "len = ", len(bb), " kind = ", kind(bb)
      write(*,*) "len = ", len(cc), " kind = ", kind(cc)
      write(*,*) "size = ", size(rr1end)
      write(*,*) "size = ", size(rr2end)
      write(*,*) "size = ", size(rr3end)
      write(*,*)
      write(*,*) "len = ", len(aaa), " kind = ", kind(aaa)
      write(*,*) "len = ", len(bbb), " kind = ", kind(bbb)
      write(*,*) "len = ", len(ccc), " kind = ", kind(ccc)
      write(*,*) "size = ", size(rr1eor)
      write(*,*) "size = ", size(rr2eor)
      write(*,*) "size = ", size(rr3eor)

      end
