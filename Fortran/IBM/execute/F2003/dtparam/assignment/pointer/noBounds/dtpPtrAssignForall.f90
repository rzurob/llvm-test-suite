!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpPtrAssignForall
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-01-21
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment without Lower Bounds Specification or Remap
!*
!*  SECONDARY FUNCTIONS TESTED : FORALL statement
!*
!*  REFERENCE                  : Feature Number 360669
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Declare targets of fixed length parameters and pointers with deferred length
!*  parameters, and try assigning targets with different lengths to pointers, to
!*  verify that the pointers refer to the correct information.  We're killing
!*  two birds with one stone, too: we're also using component pointers and
!*  pointing to components.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program dtpPtrAssignForall

  implicit none

  type dk(k)
     integer, kind :: k
     integer(k) :: idat = 0
  end type dk

  type dl(l)
     integer, len  :: l
     character(l) :: cdat = "----"
  end type dl

  type dboth (k, l)
     integer, kind :: k
     integer, len  :: l
     type (dk(k)) :: dk1
     type (dk(k)) :: dk2
     type (dk(k)), pointer :: dkp  => null()
     type (dl(l)) :: dl1
     type (dl(l)) :: dl2
     type (dl(l)), pointer :: dlp  => null()
     type (dl(:)), pointer :: dlqp => null()
  end type dboth

  integer :: i

  type(dk(8)), target  :: tk8, tk8a
  type(dk(4)), target  :: tk4, tk4a

  type(dl(7)), target  :: tl7, tl7a
  type(dl(4)), target  :: tl4, tl4a
  type(dl(8)), target  :: tl8, tl8a

  type(dboth(4,4)), target :: b44(5)
  type(dboth(4,8)), target :: b48(5)
  type(dboth(8,4)), target :: b84(5)
  type(dboth(8,8)), target :: b88(5)

  tk4  = dk(4)(20002)
  tk4a = dk(4)(10001)
  tk8  = dk(8)(2000200020002_8)
  tk8a = dk(8)(1000100010001_8)
  tl4  = dl(4)('quer')
  tl7  = dl(7)('commons')
  tl8  = dl(8)('partpalc')
  tl4a = dl(4)('wxyz')
  tl7a = dl(7)('snommoc')
  tl8a = dl(8)('claptrap')

  b44 = [(dboth(4,4)(dk(4)(101+i),dk(4)(102+i),tk4,dl(4)('aceg'),dl(4)('ikmo'),tl4,tl7), i=1,5)]
  b48 = [(dboth(4,8)(dk(4)(101-i),dk(4)(102-i),tk4,dl(8)(repeat(achar(iachar('q')+i),8)),dl(8)('ijklmnop'),tl8,tl7), i=1,5)]
  b84 = [(dboth(8,4)(dk(8)(101*i),dk(8)(102*i),tk8,dl(4)(repeat(achar(iachar('a')+i),4)),dl(4)('ijkl'),tl4,tl7), i=1,5)]
  b88 = [(dboth(8,8)(dk(8)(101/i),dk(8)(102/i),tk8,dl(8)(repeat(achar(iachar('m')+i),8)),dl(8)('ijklmnop'),tl8,tl7), i=1,5)]

  print *, (b44(i)%k, b44(i)%l, b44(i)%dk1, b44(i)%dk2, b44(i)%dkp%k, b44(i)%dkp%idat, b44(i)%dl1, b44(i)%dl2, b44(i)%dlp%l, b44(i)%dlp%cdat, b44(i)%dlqp%l, b44(i)%dlqp%cdat, i=1,5)
  print *, (b48(i)%k, b48(i)%l, b48(i)%dk1, b48(i)%dk2, b48(i)%dkp%k, b48(i)%dkp%idat, b48(i)%dl1, b48(i)%dl2, b48(i)%dlp%l, b48(i)%dlp%cdat, b48(i)%dlqp%l, b48(i)%dlqp%cdat, i=1,5)
  print *, (b84(i)%k, b84(i)%l, b84(i)%dk1, b84(i)%dk2, b84(i)%dkp%k, b84(i)%dkp%idat, b84(i)%dl1, b84(i)%dl2, b84(i)%dlp%l, b84(i)%dlp%cdat, b84(i)%dlqp%l, b84(i)%dlqp%cdat, i=1,5)
  print *, (b88(i)%k, b88(i)%l, b88(i)%dk1, b88(i)%dk2, b88(i)%dkp%k, b88(i)%dkp%idat, b88(i)%dl1, b88(i)%dl2, b88(i)%dlp%l, b88(i)%dlp%cdat, b88(i)%dlqp%l, b88(i)%dlqp%cdat, i=1,5)

  print *
  forall (i = 1:5, b44(i)%dk1%idat > 103)
     b44(i)%dk2%idat = b44(i)%dk1%idat
     b44(i)%dkp => tk4a
  end forall

  forall (i = 1:5, b84(i)%dk1%idat <= 299)
     b84(i)%dk2%idat = b84(i)%dk1%idat
     b84(i)%dkp => tk8a
  end forall

  forall (i = 1:5, b48(i)%dl1%cdat < 's')
     b48(i)%dl2%cdat = b48(i)%dl1%cdat
     b48(i)%dlp => tl8a
  end forall

  forall (i = 1:5, b88(i)%dl1%cdat < 'r')
     b88(i)%dl2%cdat = b88(i)%dl1%cdat
     b88(i)%dlqp => tl7a
  end forall

  print *, (b44(i)%k, b44(i)%l, b44(i)%dk1, b44(i)%dk2, b44(i)%dkp%k, b44(i)%dkp%idat, b44(i)%dl1, b44(i)%dl2, b44(i)%dlp%l, b44(i)%dlp%cdat, b44(i)%dlqp%l, b44(i)%dlqp%cdat, i=1,5)
  print *, (b48(i)%k, b48(i)%l, b48(i)%dk1, b48(i)%dk2, b48(i)%dkp%k, b48(i)%dkp%idat, b48(i)%dl1, b48(i)%dl2, b48(i)%dlp%l, b48(i)%dlp%cdat, b48(i)%dlqp%l, b48(i)%dlqp%cdat, i=1,5)
  print *, (b84(i)%k, b84(i)%l, b84(i)%dk1, b84(i)%dk2, b84(i)%dkp%k, b84(i)%dkp%idat, b84(i)%dl1, b84(i)%dl2, b84(i)%dlp%l, b84(i)%dlp%cdat, b84(i)%dlqp%l, b84(i)%dlqp%cdat, i=1,5)
  print *, (b88(i)%k, b88(i)%l, b88(i)%dk1, b88(i)%dk2, b88(i)%dkp%k, b88(i)%dkp%idat, b88(i)%dl1, b88(i)%dl2, b88(i)%dlp%l, b88(i)%dlp%cdat, b88(i)%dlqp%l, b88(i)%dlqp%cdat, i=1,5)

end program dtpPtrAssignForall
