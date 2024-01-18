! GM DTP extension using:
! ftcx_dtp -qk -ql -qdefaultpv -qdeferredlp -qreuse=none /tstdev/F2003/ace/types/derived/acetdt59.f

!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : acetdt59kl_dlp_dpv
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acetdt59
!*                               by David Forster)
!*  DATE                       : 2008-01-23 (original: 2006-11-24)
!*  ORIGIN                     : Compiler Development,
!*                               IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancements
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf2003)
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  Repeat the integer tests on array sections with a derived type.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetdt59kl_dlp_dpvmod

  implicit none
  type item(l1,k1)    ! (20,4)
     integer, kind :: k1
     integer, len  :: l1
     integer(k1)   :: val
  end type item

  type derived(k2,l2)    ! (4,20)
     integer, kind                 :: k2
     integer, len                  :: l2
     type(item(l2,k2))             :: ivals(5)
     type(item(:,k2)), allocatable :: iavals(:)
  end type derived

end module acetdt59kl_dlp_dpvmod


program acetdt59kl_dlp_dpv

  use acetdt59kl_dlp_dpvmod
  implicit none
  type (item(20,4)) :: iarr(5)
  integer     :: inx(5), i
  type (item(:,4)), allocatable :: iarra(:)
  type (derived(4,20)) :: dt

  iarr = [item(20,4):: (item(20,4)(i ** 2), i = 1,5)]
  print *, ' 1:', iarr
  iarr = [item(20,4):: iarr]
  print *, ' 2:', iarr
  iarr = [item(20,4):: iarr(5:1:-1)]
  print *, ' 3:', iarr
  inx  = [integer:: 5, 1, 4, 3, 2]
  print *, ' 4:', [item(20,4):: (iarr(inx(i)), i=1,5)]
  iarr = [item(20,4):: (iarr(inx(i)), i=1,5)]
  print *, ' 5:', iarr
  iarr(1:3) = [item(20,4):: iarr(3:5)]
  print *, ' 6:', iarr
  iarr = [item(20,4):: item(20,4)(1), item(20,4)(2), item(20,4)(3), item(20,4)(4), item(20,4)(5)]
  iarr(1:5:2) = [item(20,4):: iarr(5:3:-1)]
  print *, ' 7:', iarr
  iarr = [item(20,4):: item(20,4)(1), item(20,4)(2), item(20,4)(3), item(20,4)(4), item(20,4)(5)]
  iarr(5:1:-2) = [item(20,4):: iarr(5:3:-1)]                    ! iarr(5,3,1) = 5,4,3 => 3 2 4 4 5
  print *, ' 8:', iarr
  iarr = [item(20,4):: item(20,4)(1), item(20,4)(2), item(20,4)(3), item(20,4)(4), item(20,4)(5)]
  iarr(iarr(5:1:-2)%val) = [item(20,4):: iarr([integer:: 4, 1, 2])] ! iarr(5,3,1) = 4,1,2 => 2 2 1 4 4
  print *, ' 9:', iarr
  iarr = [item(20,4):: item(20,4)(1), item(20,4)(2), item(20,4)(3), item(20,4)(4), item(20,4)(5)]
  iarr([integer:: (iarr(i:i-1:-1)%val, i=2,5,3)]) = [item(20,4):: iarr(5:2:-1)] ! iarr(2,1,5,4) = 5,4,3,2 => 4 5 3 2 3
  print *, '10:', iarr

  ! implicit allocation the first time, thereafter it should reuse space
  iarra = [item(20,4):: (item(20,4)(i ** 2), i = 1,5)]
  print *, '11:', iarra
  iarra = [item(20,4):: iarra]
  print *, '12:', iarra
  iarra = [item(20,4):: iarra(5:1:-1)]
  print *, '13:', iarra
  inx  = [integer:: 5, 1, 4, 3, 2]
  print *, '14:', [item(20,4):: (iarra(inx(i)), i=1,5)]
  iarra = [item(20,4):: (iarra(inx(i)), i=1,5)]
  print *, '15:', iarra
  iarra(1:3) = [item(20,4):: iarra(3:5)]
  print *, '16:', iarra
  iarra = [item(20,4):: item(20,4)(1), item(20,4)(2), item(20,4)(3), item(20,4)(4), item(20,4)(5)]
  iarra(1:5:2) = [item(20,4):: iarra(5:3:-1)]
  print *, '17:', iarra
  iarra = [item(20,4):: item(20,4)(1), item(20,4)(2), item(20,4)(3), item(20,4)(4), item(20,4)(5)]
  iarra(5:1:-2) = [item(20,4):: iarra(5:3:-1)]
  print *, '18:', iarra
  iarra = [item(20,4):: item(20,4)(1), item(20,4)(2), item(20,4)(3), item(20,4)(4), item(20,4)(5)]
  iarra(iarra(5:1:-2)%val) = [item(20,4):: iarra([integer:: 4, 1, 2])] ! iarra(5,3,1) = 4,1,2 => 2 2 1 4 4
  print *, '19:', iarra
  iarra = [item(20,4):: item(20,4)(1), item(20,4)(2), item(20,4)(3), item(20,4)(4), item(20,4)(5)]
  iarra([integer:: (iarra(i:i-1:-1)%val, i=2,5,3)]) = [item(20,4):: iarra(5:2:-1)] ! iarra(2,1,5,4) = 5,4,3,2 => 4 5 3 2 3
  print *, '20:', iarra

  ! Repeat, but changing the size each time, to force reallocation:
  iarra = [item(20,4):: (item(20,4)(i ** 2), i = 1,6)]
  print *, '21:', iarra
  iarra = [item(20,4):: iarra(5:2:-1), iarra(2:5)]
  print *, '22:', iarra
  inx  = [integer:: 7, 4, 1, 3, -1] ! only use the first four
  print *, '23:', [item(20,4):: (iarra(inx(i)), i=1,4), item(20,4)(1)]
  iarra = [item(20,4):: (iarra(inx(i)), i=1,4), item(20,4)(1)]
  print *, '24:', iarra
  iarra(1:3) = [item(20,4):: iarra(3:5)]
  print *, '25:', iarra
  iarra = [item(20,4):: item(20,4)(1), item(20,4)(2), item(20,4)(3), item(20,4)(4), item(20,4)(5), item(20,4)(6)]
  iarra(1:5:2) = [item(20,4):: iarra(5:3:-1)]
  print *, '26:', iarra
  iarra = [item(20,4):: item(20,4)(1), item(20,4)(2), item(20,4)(3), item(20,4)(4), item(20,4)(5)]
  iarra(5:1:-2) = [item(20,4):: iarra(5:3:-1)]                     ! iarra(5,3,1) = 5,4,3 => 3 2 4 4 5
  print *, '27:', iarra
  iarra = [item(20,4):: item(20,4)(1), item(20,4)(2), item(20,4)(3), item(20,4)(4), item(20,4)(5), item(20,4)(6)]
  iarra(iarra(5:1:-2)%val) = [item(20,4):: iarra([integer:: 4, 1, 2])] ! iarra(5,3,1) = 4,1,2 => 2 2 1 4 4 6
  print *, '28:', iarra
  iarra = [item(20,4):: item(20,4)(1), item(20,4)(2), item(20,4)(3), item(20,4)(4), item(20,4)(5)]
  iarra([integer:: (iarra(i:i-1:-1)%val, i=2,5,3)]) = [item(20,4):: iarra(5:2:-1)] ! iarra(2,1,5,4) = 5,4,3,2 => 4 5 3 2 3
  print *, '29:', iarra

  ! try a sampling of tests accessing a field in a derived type object:
  dt%ivals = [item(20,4):: (item(20,4)(i ** 2), i = 1,5)]
  print *, '30:', dt%ivals
  dt%ivals = [item(20,4):: dt%ivals]
  print *, '31:', dt%ivals
  dt%ivals = [item(20,4):: dt%ivals(5:1:-1)]
  print *, '32:', dt%ivals
  inx  = [integer:: 5, 1, 4, 3, 2]
  print *, '33:', [item(20,4):: (dt%ivals(inx(i)), i=1,5)]
  dt%ivals = [item(20,4):: (dt%ivals(inx(i)), i=1,5)]
  print *, '34:', dt%ivals
  dt%ivals(1:3) = [item(20,4):: dt%ivals(3:5)]
  print *, '35:', dt%ivals
  dt%ivals = [item(20,4):: item(20,4)(1), item(20,4)(2), item(20,4)(3), item(20,4)(4), item(20,4)(5)]
  dt%ivals(1:5:2) = [item(20,4):: dt%ivals(5:3:-1)]
  print *, '36:', dt%ivals
  dt%ivals = [item(20,4):: item(20,4)(1), item(20,4)(2), item(20,4)(3), item(20,4)(4), item(20,4)(5)]
  dt%ivals(5:1:-2) = [item(20,4):: dt%ivals(5:3:-1)]                    ! dt%ivals(5,3,1) = 5,4,3 => 3 2 4 4 5
  print *, '37:', dt%ivals
  dt%ivals = [item(20,4):: item(20,4)(1), item(20,4)(2), item(20,4)(3), item(20,4)(4), item(20,4)(5)]
  dt%ivals(dt%ivals(5:1:-2)%val) = [item(20,4):: dt%ivals([integer:: 4, 1, 2])] ! dt%ivals(5,3,1) = 4,1,2 => 2 2 1 4 4
  print *, '38:', dt%ivals
  dt%ivals = [item(20,4):: item(20,4)(1), item(20,4)(2), item(20,4)(3), item(20,4)(4), item(20,4)(5)]
  dt%ivals([integer:: (dt%ivals(i:i-1:-1)%val, i=2,5,3)]) = [item(20,4):: dt%ivals(5:2:-1)] ! dt%ivals(2,1,5,4) = 5,4,3,2 => 4 5 3 2 3
  print *, '39:', dt%ivals

  ! and repeat for an allocatable component:
  dt%iavals = [item(20,4):: (item(20,4)(i ** 2), i = 1,6)]
  print *, '40:', dt%iavals
  dt%iavals = [item(20,4):: dt%iavals]
  print *, '41:', dt%iavals
  dt%iavals = [item(20,4):: dt%iavals(5:2:-1), dt%iavals(2:5)]
  print *, '42:', dt%iavals
  inx  = [integer:: 7, 4, 1, 3, -1] ! only use the first four
  print *, '43:', [item(20,4):: (dt%iavals(inx(i)), i=1,4), item(20,4)(1)]
  dt%iavals = [item(20,4):: (dt%iavals(inx(i)), i=1,4), item(20,4)(1)]
  print *, '44:', dt%iavals
  dt%iavals(1:3) = [item(20,4):: dt%iavals(3:5)]
  print *, '45:', dt%iavals
  dt%iavals = [item(20,4):: item(20,4)(1), item(20,4)(2), item(20,4)(3), item(20,4)(4), item(20,4)(5), item(20,4)(6)]
  dt%iavals(1:5:2) = [item(20,4):: dt%iavals(5:3:-1)]
  print *, '46:', dt%iavals
  dt%iavals = [item(20,4):: item(20,4)(1), item(20,4)(2), item(20,4)(3), item(20,4)(4), item(20,4)(5)]
  dt%iavals(5:1:-2) = [item(20,4):: dt%iavals(5:3:-1)]                     ! dt%iavals(5,3,1) = 5,4,3 => 3 2 4 4 5
  print *, '47:', dt%iavals
  dt%iavals = [item(20,4):: item(20,4)(1), item(20,4)(2), item(20,4)(3), item(20,4)(4), item(20,4)(5), item(20,4)(6)]
  dt%iavals(dt%iavals(5:1:-2)%val) = [item(20,4):: dt%iavals([integer:: 4, 1, 2])] ! dt%iavals(5,3,1) = 4,1,2 => 2 2 1 4 4 6
  print *, '48:', dt%iavals
  dt%iavals = [item(20,4):: item(20,4)(1), item(20,4)(2), item(20,4)(3), item(20,4)(4), item(20,4)(5)]
  dt%iavals([integer:: (dt%iavals(i:i-1:-1)%val, i=2,5,3)]) = [item(20,4):: dt%iavals(5:2:-1)] ! dt%iavals(2,1,5,4) = 5,4,3,2 => 4 5 3 2 3
  print *, '49:', dt%iavals

end program acetdt59kl_dlp_dpv
