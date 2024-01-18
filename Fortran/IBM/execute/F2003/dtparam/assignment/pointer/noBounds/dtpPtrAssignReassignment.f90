!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpPtrAssignReassignment
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-01-21
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment without Lower Bounds Specification or Remap
!*
!*  SECONDARY FUNCTIONS TESTED : verify that pointers with deferred length parameters reflect parameters of target, even after reassignment
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
!*  verify that the pointers refer to the correct information.
!*  In the reassignment, we make sure we also use pointers to components and
!*  components as pointers.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program dtpPtrAssignReassignment

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

  type(dk(4)), pointer :: pk4, pk4a
  type(dk(8)), pointer :: pk8, pk8a
  type(dl(4)), pointer :: pl4, pl4a
  type(dl(8)), pointer :: pl8, pl8a
  type(dl(:)), pointer :: plq, plqa, plqb, plqc

  type(dboth(4,4)), pointer :: pb44
  type(dboth(4,8)), pointer :: pb48
  type(dboth(8,4)), pointer :: pb84
  type(dboth(8,8)), pointer :: pb88
  type(dboth(4,:)), pointer :: pb4q
  type(dboth(8,:)), pointer :: pb8q

  type(dk(4)), target  :: tk4, tk4a
  type(dk(8)), target  :: tk8, tk8a
  type(dl(4)), target  :: tl4, tl4a
  type(dl(8)), target  :: tl8, tl8a
  type(dl(7)), target  :: tl7, tl7a, tl7b, tl7c
  type(dboth(4,4)), target  :: tb44
  type(dboth(4,8)), target  :: tb48
  type(dboth(8,4)), target  :: tb84
  type(dboth(8,8)), target  :: tb88

  integer(4), pointer :: ip4
  integer(8), pointer :: ip8

  pk4  => tk4
  pk4a => tk4a
  pk8  => tk8
  pk8a => tk8a
  pl4  => tl4
  pl4a => tl4a
  pl8  => tl8
  pl8a => tl8a
  plq  => tl7
  plqa => tl7a
  plqb => tl7b
  plqc => tl7c
  pb44 => tb44
  pb48 => tb48
  pb84 => tb84
  pb88 => tb88

  pk4  = dk(4)(101101101)
  pk4a = dk(4)(102102102)
  pk8  = dk(8)(1234567890987654321_8)
  pk8a = dk(8)(23456789098765432_8)
  pl4  = dl(4)('abcde')
  pl4a = dl(4)('ABCDE')
  pl8  = dl(8)('fghijklmn')
  pl8a = dl(8)('FGHIJKLMN')
  plq  = dl(7)('Cavalleria')
  plqa = dl(7)('Rusticana')
  plqb = dl(7)('Ruggero')
  plqc = dl(7)('Leoncavallo')

  pb44 = dboth(4,4)(dk(4)(123456789_4),dk(4)(987654321_4),pk4, &
                     dl(4)('tony'),dl(4)('vivaldi'),pl4,plq)
  pb48 = dboth(4,8)(dk(4)(112233445_4),dk(4)(667788990_4),pk4a, &
                     dl(8)('Antonio'),dl(8)('Salieri'),pl8,plqa)
  pb84 = dboth(8,4)(dk(8)(11223344544332211_8),dk(8)(1122334455667788990_8),pk8, &
                     dl(4)('tiny'),dl(4)('vavildi'),pl4a,plqb)
  pb88 = dboth(8,8)(dk(8)(5443322112233445_8),dk(8)(998877667788990_8),pk8a, &
                     dl(8)('TinyTony'),dl(8)('SillySally'),pl8a,plqc)

  pb4q => pb44
  pb8q => pb84

  print *, pk4, pk4%k, pk4%idat
  print *, pk4a, pk4a%k, pk4a%idat
  print *, pk8, pk8%k, pk8%idat
  print *, pk8a, pk8a%k, pk8a%idat

  print *, pl4, pl4%l, pl4%cdat
  print *, pl4a, pl4a%l, pl4a%cdat
  print *, pl8, pl8%l, pl8%cdat
  print *, pl8a, pl8a%l, pl8a%cdat

  print *, plq, plq%l, plq%cdat
  print *, plqa, plqa%l, plqa%cdat
  print *, plqb, plqb%l, plqb%cdat
  print *, plqc, plqc%l, plqc%cdat

  print *, pb44%k, pb44%l, pb44%dk1, pb44%dk1%k, pb44%dk1%idat, pb44%dk2, pb44%dk2%k, pb44%dk2%idat, &
           pb44%dkp%k, pb44%dkp%idat, pb44%dl1, pb44%dl1%l, pb44%dl1%cdat, pb44%dl2, &
           pb44%dl2%l, pb44%dl2%cdat, pb44%dlp%l, pb44%dlp%cdat, pb44%dlqp%l, pb44%dlqp%cdat
  print *, pb48%k, pb48%l, pb48%dk1, pb48%dk1%k, pb48%dk1%idat, pb48%dk2, pb48%dk2%k, pb48%dk2%idat, &
           pb48%dkp%k, pb48%dkp%idat, pb48%dl1, pb48%dl1%l, pb48%dl1%cdat, pb48%dl2, &
           pb48%dl2%l, pb48%dl2%cdat, pb48%dlp%l, pb48%dlp%cdat, pb48%dlqp%l, pb48%dlqp%cdat
  print *, pb84%k, pb84%l, pb84%dk1, pb84%dk1%k, pb84%dk1%idat, pb84%dk2, pb84%dk2%k, pb84%dk2%idat, &
           pb84%dkp%k, pb84%dkp%idat, pb84%dl1, pb84%dl1%l, pb84%dl1%cdat, pb84%dl2, &
           pb84%dl2%l, pb84%dl2%cdat, pb84%dlp%l, pb84%dlp%cdat, pb84%dlqp%l, pb84%dlqp%cdat
  print *, pb88%k, pb88%l, pb88%dk1, pb88%dk1%k, pb88%dk1%idat, pb88%dk2, pb88%dk2%k, pb88%dk2%idat, &
           pb88%dkp%k, pb88%dkp%idat, pb88%dl1, pb88%dl1%l, pb88%dl1%cdat, pb88%dl2, &
           pb88%dl2%l, pb88%dl2%cdat, pb88%dlp%l, pb88%dlp%cdat, pb88%dlqp%l, pb88%dlqp%cdat
  print *, pb4q%k, pb4q%l, pb4q%dk1, pb4q%dk1%k, pb4q%dk1%idat, pb4q%dk2, pb4q%dk2%k, pb4q%dk2%idat, &
           pb4q%dkp%k, pb4q%dkp%idat, pb4q%dl1, pb4q%dl1%l, pb4q%dl1%cdat, pb4q%dl2, &
           pb4q%dl2%l, pb4q%dl2%cdat, pb4q%dlp%l, pb4q%dlp%cdat, pb4q%dlqp%l, pb4q%dlqp%cdat

  print *, pb8q%k, pb8q%l, pb8q%dk1, pb8q%dk1%k, pb8q%dk1%idat, pb8q%dk2, pb8q%dk2%k, pb8q%dk2%idat, &
           pb8q%dkp%k, pb8q%dkp%idat, pb8q%dl1, pb8q%dl1%l, pb8q%dl1%cdat, pb8q%dl2, &
           pb8q%dl2%l, pb8q%dl2%cdat, pb8q%dlp%l, pb8q%dlp%cdat, pb8q%dlqp%l, pb8q%dlqp%cdat

  ip4 => pb44%dk1%idat  ! intrinsic pointer to component
  ip8 => pb88%dkp%idat  ! indirects to tk8a%idat
  pb4q%dlp => pb44%dl1  ! points within self
  pb88%dlqp => pb4q%dl2 ! changes length from 7 to 4

  print *, associated(ip4, pb44%dk1%idat), associated(ip8, pb88%dkp%idat), associated(ip8, tk8a%idat), &
           associated(pb4q%dlp, pb44%dl1), associated(pb88%dlqp, pb4q%dl2)

  ip4 = 123123123
  ip8 = 678678678678678678_8
  pb44%dlp%cdat(2:3) = 'zz'
  pb84%dl2%cdat(2:3) = 'yy'
  pb88%dlqp%cdat(1:2) = 'xx'

  print *, pk4, pk4%k, pk4%idat
  print *, pk4a, pk4a%k, pk4a%idat
  print *, pk8, pk8%k, pk8%idat
  print *, pk8a, pk8a%k, pk8a%idat

  print *, pl4, pl4%l, pl4%cdat
  print *, pl4a, pl4a%l, pl4a%cdat
  print *, pl8, pl8%l, pl8%cdat
  print *, pl8a, pl8a%l, pl8a%cdat

  print *, plq, plq%l, plq%cdat
  print *, plqa, plqa%l, plqa%cdat
  print *, plqb, plqb%l, plqb%cdat
  print *, plqc, plqc%l, plqc%cdat

  print *, pb44%k, pb44%l, pb44%dk1, pb44%dk1%k, pb44%dk1%idat, pb44%dk2, pb44%dk2%k, pb44%dk2%idat, &
           pb44%dkp%k, pb44%dkp%idat, pb44%dl1, pb44%dl1%l, pb44%dl1%cdat, pb44%dl2, &
           pb44%dl2%l, pb44%dl2%cdat, pb44%dlp%l, pb44%dlp%cdat, pb44%dlqp%l, pb44%dlqp%cdat
  print *, pb48%k, pb48%l, pb48%dk1, pb48%dk1%k, pb48%dk1%idat, pb48%dk2, pb48%dk2%k, pb48%dk2%idat, &
           pb48%dkp%k, pb48%dkp%idat, pb48%dl1, pb48%dl1%l, pb48%dl1%cdat, pb48%dl2, &
           pb48%dl2%l, pb48%dl2%cdat, pb48%dlp%l, pb48%dlp%cdat, pb48%dlqp%l, pb48%dlqp%cdat
  print *, pb84%k, pb84%l, pb84%dk1, pb84%dk1%k, pb84%dk1%idat, pb84%dk2, pb84%dk2%k, pb84%dk2%idat, &
           pb84%dkp%k, pb84%dkp%idat, pb84%dl1, pb84%dl1%l, pb84%dl1%cdat, pb84%dl2, &
           pb84%dl2%l, pb84%dl2%cdat, pb84%dlp%l, pb84%dlp%cdat, pb84%dlqp%l, pb84%dlqp%cdat
  print *, pb88%k, pb88%l, pb88%dk1, pb88%dk1%k, pb88%dk1%idat, pb88%dk2, pb88%dk2%k, pb88%dk2%idat, &
           pb88%dkp%k, pb88%dkp%idat, pb88%dl1, pb88%dl1%l, pb88%dl1%cdat, pb88%dl2, &
           pb88%dl2%l, pb88%dl2%cdat, pb88%dlp%l, pb88%dlp%cdat, pb88%dlqp%l, pb88%dlqp%cdat
  print *, pb4q%k, pb4q%l, pb4q%dk1, pb4q%dk1%k, pb4q%dk1%idat, pb4q%dk2, pb4q%dk2%k, pb4q%dk2%idat, &
           pb4q%dkp%k, pb4q%dkp%idat, pb4q%dl1, pb4q%dl1%l, pb4q%dl1%cdat, pb4q%dl2, &
           pb4q%dl2%l, pb4q%dl2%cdat, pb4q%dlp%l, pb4q%dlp%cdat, pb4q%dlqp%l, pb4q%dlqp%cdat

  print *, pb8q%k, pb8q%l, pb8q%dk1, pb8q%dk1%k, pb8q%dk1%idat, pb8q%dk2, pb8q%dk2%k, pb8q%dk2%idat, &
           pb8q%dkp%k, pb8q%dkp%idat, pb8q%dl1, pb8q%dl1%l, pb8q%dl1%cdat, pb8q%dl2, &
           pb8q%dl2%l, pb8q%dl2%cdat, pb8q%dlp%l, pb8q%dlp%cdat, pb8q%dlqp%l, pb8q%dlqp%cdat

  print *, 'done'

end program dtpPtrAssignReassignment
