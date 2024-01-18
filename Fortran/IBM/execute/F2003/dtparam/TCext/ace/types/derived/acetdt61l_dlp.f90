! GM DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/ace/types/derived/acetdt61.f

!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : acetdt61l_dlp
!*
!*                               by David Forster)
!*  DATE                       : 2008-01-23 (original: 2006-11-26)
!*  ORIGIN                     : Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancements
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  Define a matrix via a rank remapped pointer and an AC.
!*  Use same in an AC.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetdt61l_dlpmod

  implicit none
  type dt(l1,k1)    ! (20,4)
     integer, kind :: k1
     integer, len  :: l1
     real(k1)      :: val
  end type dt

end module acetdt61l_dlpmod


program acetdt61l_dlp

  use acetdt61l_dlpmod
  implicit none
  type(dt(20,4)), allocatable, target :: base(:)
  type(dt(:,4)), pointer :: matrix(:,:), diagonal(:)
  integer :: n, i

  ! "pointer rank remapping"
  n = 6
  allocate(base(n*n))
  matrix(1:n,1:n) => base ! rank remapping
  diagonal => base(::n+1)

  base = [dt(20,4):: (dt(20,4)(real(i/10.0)), i = 1,36)]
  diagonal = [dt(20,4):: (dt(20,4)(real(i*i)), i=1,n)]
  print *, base

  diagonal = [dt(20,4):: (diagonal(i), i=n,1,-1)]
  print *, base

  matrix(3,:) = [dt(20,4):: matrix(2,:)]
  print *, base

  matrix(2,:) = [dt(20,4):: (matrix(1,i), i=1,n)]
  print *, base

  matrix(4,:) = [dt(20,4):: diagonal]
  print *, base

  diagonal = [dt(20,4):: (dt(20,4)(sum(matrix(i,:)%val)), i=1,6)]
  print *, base

  diagonal = [dt(20,4):: (dt(20,4)(sum(matrix(:,i)%val)), i=1,6)]
  print *, base

end program acetdt61l_dlp
