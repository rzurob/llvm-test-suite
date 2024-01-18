! GM DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/ace/diag/types/intrinsics/acetint22d.f

!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : acetint22dl
!*
!*                               by David Forster)
!*  DATE                       : 2007-12-03 (original: 2006-06-08)
!*  ORIGIN                     : Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancements
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : intrinsic type
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that intrinsic type specifiers enforce correct KIND values.
!*  We look at the correctness of the value and that of the dynamic type and
!*  kind of the constructed array in a separate test.
!*  This set of tests rounds out those in test cases acetint10[cilrz]d; tests
!*  in acetint26d round out tests for the character type specifier.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod
  type double(l1,k1)    ! (20,4)
     integer, kind :: k1
     integer, len  :: l1
     integer(k1)   :: val = 4
  end type
end module mod

program acetint22dl

  use mod
  implicit none
  type (double(20,4)), parameter :: dp4 = double(20,4)(4), kind = double(20,4)(4), dp3 = double(20,4)(3)

  character :: charr(3)
  logical   :: larr(2)
  integer   :: iarr(3), i
  complex   :: zarr(1)
  real      :: rarr(3)
  double precision :: darr(3)

  integer, parameter   :: fixed(2) = (/4, 8/)
  character, parameter :: c2 = 'a'

  ! These should be okay (included to establish a baseline):
  iarr  = (/integer(kind=dp4%val):: 1,2,3/)
  iarr  = (/integer(kind=kind%val):: 1,2,3/)
  ! Repeated, sans "kind="
  iarr  = (/integer(dp4%val):: 1,2,3/)
  iarr  = (/integer(kind%val):: 1,2,3/)
  ! Repeated, with other types:
  rarr  = (/real(kind=kind%val) :: 1.2,2.3,3.4/)
  zarr  = (/complex(kind=kind%val):: (1.2,2.3)/)
  zarr  = (/complex(kind%val):: (1.2,2.3)/)
  larr  = (/logical(kind=kind%val):: .false.,.true./)
  ! And print one or two:
  print *, [double precision:: ]
  print *, [double complex:: ]


  ! These are bad:
  iarr  = (/integer(kind=0):: 1,2,3/)       ! kind must be integer, not character
  iarr  = (/integer(kind=i):: 1,2,3/)       ! kind must be evaluated at compile time
  iarr  = (/integer(kind=c2):: 1,2,3/)      ! kind must be integer, not character
  iarr  = (/integer(kind=dp4):: 1,2,3/)     ! kind must be integer, not derived type
  iarr  = (/integer(kind=kind):: 1,2,3/)    ! kind must be integer, not derived type (despite possible name confusion)
  iarr  = (/integer(kind=fixed):: 1,2,3/)   ! kind must be integer, not array
  iarr  = (/integer(kind=dp3%val):: 1,2,3/) ! int parm, but kind of 3 is not allowed

  ! repeat the above, sans "kind="
  iarr  = (/integer(0):: 1,2,3/)
  iarr  = (/integer(i):: 1,2,3/)
  iarr  = (/integer(c2):: 1,2,3/)
  iarr  = (/integer(dp4):: 1,2,3/)
  iarr  = (/integer(kind):: 1,2,3/)
  iarr  = (/integer(fixed):: 1,2,3/)
  iarr  = (/integer(dp3%val):: 1,2,3/)

  ! and again, with a "print" context
  print *, [integer(0):: 1,2,3]
  print *, [integer(i):: 1,2,3]
  print *, [integer(c2):: 1,2,3]
  print *, [integer(dp4):: 1,2,3]
  print *, [integer(kind):: 1,2,3]
  print *, [integer(fixed):: 1,2,3]
  print *, [integer(dp3%val):: 1,2,3]

  ! Repeat selected items for types other than integer (above) and character (in 26d)
  rarr  = [real(0) :: 1.2,2.3,3.4]
  zarr  = [complex(kind=i):: (1.2,2.3)]
  larr  = [logical(kind=100031):: .false.,.true.]

  rarr  = [real(c2) :: 1.2,2.3,3.4]
  zarr  = [complex(kind=dp4):: (1.2,2.3)]
  larr  = [logical(kind=kind):: .false.,.true.]

  rarr  = [real(fixed):: 1.2,2.3,3.4]
  zarr  = [complex(kind=dp3%val):: (1.2,2.3)]

  print *, [double precision(8):: ] ! no kind is allowed
  print *, [double complex(8):: ]   ! no kind is allowed

end program acetint22dl
