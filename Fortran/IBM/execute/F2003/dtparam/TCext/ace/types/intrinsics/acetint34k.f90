!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : acetint34k
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acetint34
!*                               by David Forster)
!*  DATE                       : 2008-01-25 (original: 2006-10-31)
!*  ORIGIN                     : Compiler Development,
!*                               IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters (+ Array Constructor
!*                               Enhancements)
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement vector
!*                               subscript in LHS of assignment statement
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf2003)
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : vector subscript
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  Create arrays and use various types of AC's to index into them on the left-
!*  hand side of assignment statements.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetint34mod

  implicit none
  type derived (kderived_1) ! kderived_1=4
     integer, kind :: kderived_1
     integer(kderived_1) :: val
  end type derived

end module acetint34mod


program acetint34k

  use acetint34mod
  implicit none

  integer:: iarr(5), sub(3)
  integer:: i, j, k

  iarr = [integer:: (i-3, i=1,5)]
  sub  = [integer:: 5, 1, 3]

  print *, ' 1:', iarr ! -2,-1,0,1,2

  print *, ' 2:', iarr(sub) ! 2,-2,0
  iarr (sub) = [integer:: 100, 200, 300]
  print *, ' 3:', iarr      !  200, -1, 300, 1, 100

  iarr = [integer:: (i-3, i=1,5)]
  iarr ([integer:: 2, 4]) = [integer:: 400, 500]
  print *, ' 4:', iarr      !  -2, 400, 0, 500, 2

  iarr = [integer:: (i-3, i=1,5)]
  iarr (sub) = 63
  print *, ' 5:', iarr      !  63, -1, 63, 1, 63

  iarr = [integer:: (i-3, i=1,5)]
  iarr ([integer:: 2, 4]) = 64
  print *, ' 6:', iarr      !  -2, 64, 0, 64, -2

  ! IBM allows the use of reals and doubles as an extension, but we won't test those directly.
  ! Instead, we'll wrap an integer AC around them.
  iarr = [integer:: (i-3, i=1,5)]
  iarr ([integer:: 3.2, 4.4]) = 65
  print *, ' 7:', iarr      !  -2, -1, 65, 65, 2

  iarr = [integer:: (i-3, i=1,5)]
  iarr ([integer:: 3.2, 4.4] - 1) = 66
  print *, ' 8:', iarr      !  -1, 66, 66, 1, 2

  iarr = [integer:: (i-3, i=1,5)]
  iarr ([integer:: 3.2, 4.4] * [real:: 0.5, 0.75]) = 67
  print *, ' 9:', iarr      !   57, 2, 67, 3, -1

  iarr = [integer:: (i-3, i=1,5)]
  iarr ([integer:: [double precision:: 3.0d0, 2.0d0, 1.0d0]]) = iarr ([integer:: 5, 4, 3]) ! -1, 3, 9
  print *, '10:', iarr      !   0, 1, 2, 1, 2

  iarr = [integer:: (i-3, i=1,5)]
  iarr ([integer:: 3.0d0, 2.0d0, 1.0d0]) = iarr ([integer:: 5, 4, 3]) ! 2, 1, 0
  print *, '11:', iarr      ! 0 1 2 1 2

  iarr = [integer:: (i-3, i=1,5)]
  iarr ([integer:: 1.0d0, 2.0d0, 3.0d0]) = iarr ([integer:: 2, 3, 1]) ! -1, 0, -2
  print *, '12:', iarr      ! -1 0 -2 1 2

  iarr = [integer:: (i-3, i=1,5)]
  iarr ([integer::]) = 100
  print *, '13:', iarr      ! -1 0 -2 1 2

  iarr = [integer:: (i-3, i=1,5)]
  iarr([integer:: [double precision:: 1d0, 3d0]]) = 67
  print *, '14:', iarr

  ! Complex is *not* allowed, even by the IBM extension, but we can wrap an integer AC around it:
  iarr = [integer:: (i-3, i=1,5)]
  iarr([integer:: (1.1d0,2.1d0), (3.9d0,4.6d0)]) = 66
  print *, '15:', iarr

  iarr = [integer:: (i-3, i=1,5)]
  iarr([integer:: [double complex:: (1.1d0,2.1d0), (3.1d0,4.1d0)]]) = 65
  print *, '16:', iarr

  ! Ditto character and logicals:
  iarr = [integer:: (i-3, i=1,5)]
  iarr([integer:: ichar([character:: 'b', 'd', 'f']) - iachar('a')]) = 64
  print *, '17:', iarr

  iarr = [integer:: (i-3, i=1,5)]
  iarr([integer:: merge(3,5,[logical:: .false., .true.])]) = [integer:: 63, 62]
  print *, '18:', iarr


  iarr = [integer:: (i-3, i=1,5)]
  iarr(([integer(1):: 127, 126] + 1) - 126) = [integer:: 22, 11]
  print *, '19:', iarr

  iarr = [integer:: 2,3,4,5,1]
  iarr(iarr) = [integer::10,20,30,40,50] ! 50 10 20 30 40
  print *, '20:', iarr

  iarr = [integer:: 2,3,4,5,1]
  iarr([integer:: (iarr(i),i=5,1,-1)]) = [integer::10,20,30,40,50] !  10 50 40 30 20
  print *, '21:', iarr

  ! Now use vector subscripts on the RHS:
  i = 5
  j = 1
  k =-2
  iarr = [integer:: 2,3,4,5,1]
  print *, 'RI', [integer:: (i+j,i=1,2), -k]
  print *, 'RA', iarr([integer:: (i+j,i=1,2), -k])
  print *, 'LI', [integer:: iarr(i:j:k)]
  print *, 'LA', iarr([integer:: iarr(i:j:k)])
  iarr([integer:: iarr(i:j:k)]) = iarr([integer:: (i+j,i=1,2), -k]) ! (5 3 1// 1 4 2 <= 2 3 2// 3 4 3 )// 3 3 4 4 1
  print *, '22:', iarr ! 3,3,4,4,1

end program acetint34k


! Extensions to introduce derived type parameters:
! type: derived - added parameters (kderived_1) to invoke with (4)/declare with (4) - 0 changes
