!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : acesynt05k
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acesynt05
!*                               by David Forster)
!*  DATE                       : 2007-12-06 (original: 2006-07-06)
!*  ORIGIN                     : Compiler Development,
!*                               IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*                               (+ Array Constructor Enhancements)
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement FORMATs
!*                               similar to array constructor (//)
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf2003)
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : syntax, format, array constructor
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  More tests on odd FORMATs, like acesynt05k.
!*
!*  The test is successful if the output is correct.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod

  type i5 (ki5_1) ! ki5_1=4
     integer, kind :: ki5_1
    integer(ki5_1) :: i5 = 1000
  end type i5

  interface format
     module procedure format1
     module procedure format1a
     module procedure format2
     module procedure format2a
     module procedure formati1a
     module procedure formati2a
  end interface

contains

  function format1(i5)
    type(i5(4)) :: i5 ! tcx: (4)
    integer  :: format1
    format1 = i5%i5
  end function format1

  function format1a(i5)
    type(i5(4)) :: i5(:) ! tcx: (4)
    integer  :: format1a
    format1a = sum(i5%i5)
  end function format1a

  function format2(i5,i6)
    type(i5(4)) :: i5, i6 ! tcx: (4)
    integer  :: format2
    format2 = i5%i5 + i6%i5
  end function format2

  function format2a(i5,i6)
    type(i5(4)) :: i5(:), i6(:) ! tcx: (4)
    integer  :: format2a
    format2a = sum(i5%i5 / i6%i5)
  end function format2a

  function formati1a(i5)
    integer  :: i5(:)
    integer  :: formati1a
    formati1a = sum(i5)
  end function formati1a

  function formati2a(i5,i6)
    integer  :: i5(:), i6(:)
    integer  :: formati2a
    formati2a = sum(i5 * i6)
  end function formati2a

end module mod

program acesynt05k

      use mod
      implicit none
      integer iarr(2), f
      type(i5(4)):: i5, i6 ! tcx: (4)

      i5%i5 = 77
      iarr = (/88,10/)
      write(6,100)
      write(6,110)
      write(6,120)

100   format(/1/)
110   format((/1/))
120   format((/1/),(/1/))
130   format((/i5::i5,i6/),(/i5::i6,i5/))
140   format((/i5::/))
150   format(i5)

      write (6,130) 4,5,6,7
      write (6,130) 4,5,6
      write (6,130) 4,5
      write (6,130) 4
      write (6,130) i5%i5
      write (6,140) format(i6)                ! 1
      write (6,150) format(i5=(/(/i5(4)::i5/)/)) ! 1a ! tcx: (4)
      write (6,150) format(i5=i6,i6=i5)       ! 2
      write (6,150) format(i5,i6)             ! 2 variant
      write (6,150) format(i6,i5)             ! 2 variant
      write (6,150) format((/i5(4)::i5,i6/),(/i5(4)::i6,i5/)) ! 2a ! tcx: (4) ! tcx: (4)

160   f = format((/3/))                       ! i1a
      print *, f
170   f = format((/4/),(/5/))                 ! i2a
      print *, f

end program acesynt05k


! Extensions to introduce derived type parameters:
! type: i5 - added parameters (ki5_1) to invoke with (4)/declare with (4) - 8 changes
