!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: diag1.f
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
!*  TEST CASE TITLE            : iostateorvals001 
!*
!*  PROGRAMMER                 : Rob Wheeler
!*  DATE                       : Jan 9, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_eor
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf
!*  REQUIRED COMPILER OPTIONS  : -qdebug=intmsg
!*
!*  DESCRIPTION                : Ensure that only -4 both yields true .  This should be run with qintsize=1,2,4,8 
			implicit none
			
			! hard coded integers to pass in
			
			integer   :: i	= -4, j, iarr(4)=(/-4,-2,0,-4/),i2arr(2,2)
			integer*1 :: i1 = -4, j1, iarr1(4)=(/-4,-2,0,-4/)
      integer*2 :: i2 = -4, j2, iarr2(4)=(/-4,-2,0,-4/)
      integer*4 :: i4 = -4, j4, iarr4(4)=(/-4,-2,0,-4/)
      integer*8 :: i8 = -4, j8, iarr8(4)=(/-4,-2,0,-4/)
      i2arr(1,1)=5
      i2arr(1,2)=-4
      i2arr(2,1)=-4
      i2arr(2,2)=-1
      
      
      !Should be true 
      write(6,*) "i = ", i
      write(6,*) "is_iostat_eor = ", is_iostat_eor(i)
      write(6,*) "i1 = ", i1
      write(6,*) "is_iostat_eor = ", is_iostat_eor(i1)
      write(6,*) "i2 = ", i2
      write(6,*) "is_iostat_eor = ", is_iostat_eor(i2)
      write(6,*) "i4 = ", i4
      write(6,*) "is_iostat_eor = ", is_iostat_eor(i4)
      write(6,*) "i8 = ", i8
      write(6,*) "is_iostat_eor = ", is_iostat_eor(i8)
      
      !Should be true for -4 false for others 
      do j=-5,5,1
      write(6,*) "j = ", j
      write(6,*) "is_iostat_eor = ", is_iostat_eor(j)
      end do
      
      do j1=-5,5,1
      write(6,*) "j1 = ", j1
      write(6,*) "is_iostat_eor = ", is_iostat_eor(j1)
      end do
      
      do j2=-5,5,1
      write(6,*) "j2 = ", j2
      write(6,*) "is_iostat_eor = ", is_iostat_eor(j2)
      end do
      
      do j4=-5,5,1
      write(6,*) "j4 = ", j4
      write(6,*) "is_iostat_eor = ", is_iostat_eor(j4)
      end do
      
      do j8=-5,5,1
      write(6,*) "j8 = ", j8
      write(6,*) "is_iostat_eor = ", is_iostat_eor(j8)
      end do
      
      !Should be T,T,F,F with this runtime option
      write(6,*) "iarr = ", iarr
      write(6,*) "is_iostat_eor = ", is_iostat_eor(iarr)
      write(6,*) "iarr1 = ", iarr1
      write(6,*) "is_iostat_eor = ", is_iostat_eor(iarr1)
      write(6,*) "iarr2 = ", iarr2
      write(6,*) "is_iostat_eor = ", is_iostat_eor(iarr2)
      write(6,*) "iarr4 = ", iarr4
      write(6,*) "is_iostat_eor = ", is_iostat_eor(iarr4)
      write(6,*) "iarr8 = ", iarr8
      write(6,*) "is_iostat_eor = ", is_iostat_eor(iarr8)
      
      !should be FFTT
      write(6,*) "i2arr = ", i2arr
      write(6,*) "is_iostat_eor = ", is_iostat_eor(i2arr)
      
      end
      
      
      
      