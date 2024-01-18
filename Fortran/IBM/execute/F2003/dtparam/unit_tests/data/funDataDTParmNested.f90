!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : funDataDTParmNested.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Zheming Gu
!*  DATE                       : May 11,2007
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : support for structure constructors containing type parameters to the DATA statement.
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 333315
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Nested type structure constructors containing type parameters to the
!*  DATA statement.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

type point (k)
  integer, kind :: k

  real(k) :: x, y
end type

type color (k,l)
  integer, kind :: k
  integer, len :: l

  integer(1) :: colorVal
end type

type, extends(point) :: colorPoint (ck,cl)
  integer, kind :: ck = 1
  integer, len  :: cl = 2

  type (color(ck,cl)) :: color
end type

type(colorPoint(4,1,2)) :: cp
integer :: i = 4
data cp /colorPoint(4,1,2)(2.0, 3.0, color(1,2)(2))/
print *,cp%k,cp%ck,cp%cl
print *,cp%color%k,cp%color%l
print *,cp%x,cp%y
print *,cp%color%colorVal
end


