!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: 
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
!*  TEST CASE TITLE            : iostatendvals001 
!*
!*  PROGRAMMER                 : Rob Wheeler
!*  DATE                       : Jan 9, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf
!*  REQUIRED COMPILER OPTIONS  : -qdebug=intmsg
!*
!*  DESCRIPTION                : Ensure that only -1 ,-2 both yields true if runtime 2003std is specified.  This should be run with qintsize=2,4,8 
			
			
			! hard coded integers to pass in
			
			integer   :: i	= -1, j=-2, iarr(4)=(/-1,-2,0,4/),i2arr(2,2)
			integer*1 :: i1 = -1, j1=-2, iarr1(4)=(/-1,-2,0,4/)
      integer*2 :: i2 = -1, j2=-2, iarr2(4)=(/-1,-2,0,4/)
      integer*4 :: i4 = -1, j4=-2, iarr4(4)=(/-1,-2,0,4/)
      integer*8 :: i8 = -1, j8=-2, iarr8(4)=(/-1,-2,0,4/)
      i2arr(1,1)=5
      i2arr(1,2)=-2
      i2arr(2,1)=0
      i2arr(2,2)=-1
      
      
      call setrteopts("iostat_end=2003std")
      
      !Should be true with this runtime option
      write(6,*) "i = ", i
      write(6,*) "is_iostat_end = ", is_iostat_end(i)
      write(6,*) "i1 = ", i1
      write(6,*) "is_iostat_end = ", is_iostat_end(i1)
      write(6,*) "i2 = ", i2
      write(6,*) "is_iostat_end = ", is_iostat_end(i2)
      write(6,*) "i4 = ", i4
      write(6,*) "is_iostat_end = ", is_iostat_end(i4)
      write(6,*) "i8 = ", i8
      write(6,*) "is_iostat_end = ", is_iostat_end(i8)
      
      !Should be true with this runtime option
      write(6,*) "j = ", j
      write(6,*) "is_iostat_end = ", is_iostat_end(j)
      write(6,*) "j1 = ", j1
      write(6,*) "is_iostat_end = ", is_iostat_end(j1)
      write(6,*) "j2 = ", j2
      write(6,*) "is_iostat_end = ", is_iostat_end(j2)
      write(6,*) "j4 = ", j4
      write(6,*) "is_iostat_end = ", is_iostat_end(j4)
      write(6,*) "j8 = ", j8
      write(6,*) "is_iostat_end = ", is_iostat_end(j8)
      
      !Should be T,T,F,F with this runtime option
      write(6,*) "iarr = ", iarr
      write(6,*) "is_iostat_end = ", is_iostat_end(iarr)
      write(6,*) "iarr1 = ", iarr1
      write(6,*) "is_iostat_end = ", is_iostat_end(iarr1)
      write(6,*) "iarr2 = ", iarr2
      write(6,*) "is_iostat_end = ", is_iostat_end(iarr2)
      write(6,*) "iarr4 = ", iarr4
      write(6,*) "is_iostat_end = ", is_iostat_end(iarr4)
      write(6,*) "iarr8 = ", iarr8
      write(6,*) "is_iostat_end = ", is_iostat_end(iarr8)
      
      !should be FFTT
      write(6,*) "i2arr = ", i2arr
      write(6,*) "is_iostat_end = ", is_iostat_end(i2arr)
      
      end
      
      
      
      