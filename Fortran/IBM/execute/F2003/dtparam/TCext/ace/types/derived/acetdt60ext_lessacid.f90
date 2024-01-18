! GM DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/ace/types/derived/acetdt60.f

!***********************************************************************
!* =====================================================================
!*
!*                               by David Forster)
!*  DATE                       : 2008-01-23 (original: 2006-11-24)
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
!*  Use array construction functions not used elsewhere.  We're already using
!*  RESHAPE in many places; MERGE has also been used, but not in its array
!*  form (e.g., merge(1,2,log) is 1 if log is true, and otherwise 2).  We
!*  have not used these yet:
!*  CSHIFT    (array, shift [,dim])           Circular shift
!*  EOSHIFT   (array, shift [,boundary, dim]) End-off shift
!*  MERGE     (tsource, fsource, mask)        Merge under mask
!*  PACK      (array, mask [,vector])         Pack into an array of rank one under a mask
!*  SPREAD    (source, dim, ncopies)          Replicates array by adding a dimension
!*  TRANSPOSE (matrix)                        Transpose a rank-2 array
!*  UNPACK    (vector, mask, field)           inverse of pack
!*
!*  NOTE:  This Test Case is a copy of acetdt60ext with some of the
!*         ac-implied-do Constructs removed.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetdt60extmod

  implicit none
  type dt(k1)    ! (4)
     integer, kind :: k1
     integer(k1)   :: ival
   contains
     procedure :: isNotEqual
     generic :: operator(.ne.) => isNotEqual
     procedure :: div
     generic :: operator(/) => div
  end type dt

contains

  elemental logical function isNotEqual(this, that)
    class (dt(4)), intent(in) :: this, that
    isNotEqual = this % ival /= that % ival
  end function isNotEqual

  elemental integer function div(this, that)
    class (dt(4)), intent(in) :: this
    integer, intent(in) :: that
    div = this % ival / that
  end function div

end module acetdt60extmod


program acetdt60ext

  use acetdt60extmod
  implicit none
  type (dt(4)) :: array(9), array19(1,9), array91(9,1), array33(3,3)
  logical, parameter :: T = .true., F = .false.
  logical :: mask (3,3)
  integer :: i


  ! Circular shift
  if (any(cshift ([dt(4):: (dt(4)(i),i=1,9)], 2) /= [dt(4):: (dt(4)(i),i=3,9), dt(4)(1), dt(4)(2)])) stop 2
  array = cshift ([dt(4):: (dt(4)(i ** 2), i=1,9)], -1)
! Replaced ac-implied-do:
! if (any(array /= [dt(4):: dt(4)(9**2), (dt(4)(i ** 2), i=1,8)])) stop 3
  if (any(array /= [dt(4):: dt(4)(9**2), dt(4)(1 ** 2), dt(4)(2 ** 2), dt(4)(3 ** 2), dt(4)(4 ** 2), dt(4)(5 ** 2), dt(4)(6 ** 2), dt(4)(7 ** 2), dt(4)(8 ** 2)])) stop 3
  print *, cshift ([dt(4):: (dt(4)(i), i=9,1,-1)], -2)

  array = [dt(4):: (dt(4)(i), i=1,9)]
  array = [dt(4):: cshift(array,shift=3)]
! Replaced ac-implied-do:
! if (any(array /= [dt(4):: (dt(4)(i), i=4,9), (dt(4)(i), i=1,3)])) stop 4
  if (any(array /= [dt(4):: dt(4)(4), dt(4)(5), dt(4)(6), dt(4)(7), dt(4)(8), dt(4)(9), dt(4)(1), dt(4)(2), dt(4)(3)])) stop 4
  array = [dt(4):: (dt(4)(i), i=1,9)]
  array = [dt(4):: cshift(reshape(array,[integer:: 3,3]), shift=[integer:: -1, 1, 0], dim=2)]
  if (any(array /= [dt(4):: dt(4)(7),dt(4)(5),dt(4)(3), dt(4)(1),dt(4)(8),dt(4)(6), dt(4)(4),dt(4)(2),dt(4)(9)])) stop 5

  array = [dt(4):: (dt(4)(i), i=1,9)]
  array = [dt(4):: cshift(reshape(array,[integer:: 3,3]), shift=[integer:: array(1:8:3)%ival], dim=2)]
  if (any(array /= [dt(4):: dt(4)(4),dt(4)(5),dt(4)(6), dt(4)(7),dt(4)(8),dt(4)(9), dt(4)(1),dt(4)(2),dt(4)(3)])) stop 6


  ! End-off shift
  if (any(eoshift ([dt(4):: (dt(4)(i),i=1,9)], 2, dt(4)(0)) /= [dt(4):: (dt(4)(i),i=3,9), dt(4)(0), dt(4)(0)])) stop 7
  array = eoshift ([dt(4):: (dt(4)(i ** 2), i=1,9)], -1, dt(4)(12))
! Replaced ac-implied-do:
! if (any(array /= [dt(4):: dt(4)(12), (dt(4)(i ** 2), i=1,8)])) stop 8
  if (any(array /= [dt(4):: dt(4)(12), dt(4)(1 ** 2), dt(4)(2 ** 2), dt(4)(3 ** 2), dt(4)(4 ** 2), dt(4)(5 ** 2), dt(4)(6 ** 2), dt(4)(7 ** 2), dt(4)(8 ** 2)])) stop 8
  print *, eoshift ([dt(4):: (dt(4)(i), i=9,1,-1)], -2, dt(4)(99))

  array = [dt(4):: (dt(4)(i), i=1,9)]
  array = [dt(4):: eoshift(array,shift=3,boundary=dt(4)(0))]
! Replaced ac-implied-do:
! if (any(array /= [dt(4):: (dt(4)(i), i=4,9), (dt(4)(0), i=1,3)])) stop 9
  if (any(array /= [dt(4):: dt(4)(4), dt(4)(5), dt(4)(6), dt(4)(7), dt(4)(8), dt(4)(9), dt(4)(0), dt(4)(0), dt(4)(0)])) stop 9
  array = [dt(4):: (dt(4)(i), i=1,9)]
  array = [dt(4):: eoshift(reshape(array,[integer:: 3,3]), shift=[integer:: -1, 1, 0], boundary = [dt(4):: array(1:8:3)], dim=2)]
  if (any(array /= [dt(4):: dt(4)(1),dt(4)(5),dt(4)(3), dt(4)(1),dt(4)(8),dt(4)(6), dt(4)(4),dt(4)(4),dt(4)(9)])) stop 10

  array = [dt(4):: (dt(4)(i), i=1,9)]
  array = [dt(4):: eoshift(reshape(array,[integer:: 3,3]), shift=[integer:: array(1:8:3)%ival], boundary = [dt(4):: array(1:3)], dim=2)]
  if (any(array /= [dt(4):: dt(4)(4),dt(4)(2),dt(4)(3), dt(4)(7),dt(4)(2),dt(4)(3), dt(4)(1),dt(4)(2),dt(4)(3)])) stop 11


  ! merge
  print *, merge([dt(4):: (dt(4)(i), i=1,5)], [dt(4):: (dt(4)(i ** 2), i=1,5)], [logical:: T, F, T, T, F]) ! 1 4 3 4 25
  if (any(merge([dt(4):: (dt(4)(i), i=1,5)], [dt(4):: (dt(4)(i ** 2), i=1,5)], [logical:: T, F, T, T, F]) /= [dt(4):: dt(4)(1), dt(4)(4), dt(4)(3), dt(4)(4), dt(4)(25)])) stop 12
  array = [dt(4):: (dt(4)(i), i=1,9)]
  array = merge([dt(4):: (array(i), i=1,9)], [dt(4):: (dt(4)(array(i)%ival ** 2), i=9,1,-1)], [logical:: T, F, T, T, F, F, F, T, T])
  if (any(array /= [dt(4):: dt(4)(1), dt(4)(64), dt(4)(3), dt(4)(4), dt(4)(25), dt(4)(16), dt(4)(9), dt(4)(8), dt(4)(9)])) stop 13


  ! PACK
  array = [dt(4):: (dt(4)(i), i=1,9)]
  print *, pack(reshape([dt(4):: (dt(4)(i ** 2), i=1,9)], [integer:: 3,3]), &
                mod(reshape(array%ival,[3,3]),3) == 0) ! 9 36 81
  array(1:3) = pack(reshape([dt(4):: (dt(4)(i ** 2), i=1,9)], [integer:: 3,3]), &
                    mod(reshape(array%ival,[3,3]),3) == 0)
  if (any(array /= [dt(4):: dt(4)(9), dt(4)(36), dt(4)(81), dt(4)(4), dt(4)(5), dt(4)(6), dt(4)(7), dt(4)(8), dt(4)(9)])) stop 14

  print *, pack(reshape([dt(4):: (dt(4)(i ** 2), i=1,9)], [integer:: 3,3]), &
                mod(reshape(array%ival,[3,3]),3) == 0, [dt(4):: (dt(4)(i ** 3), i=9,1,-1)]) ! 1 4 9 36 81 64 27 8 1
  array =  pack(reshape([dt(4):: (dt(4)(i ** 2), i=1,9)], [integer:: 3,3]), &
                mod(reshape(array%ival,[3,3]),3) == 0, [dt(4):: (dt(4)(i ** 3), i=9,1,-1)])
  if (any(array /= [dt(4):: dt(4)(1), dt(4)(4), dt(4)(9), dt(4)(36), dt(4)(81), dt(4)(64), dt(4)(27), dt(4)(8), dt(4)(1)])) stop 15

  print *, pack([dt(4):: (dt(4)(i-4), i=1,4)], .true., [dt(4):: (dt(4)(i+4), i=1,9)])
  array = pack([dt(4):: (dt(4)(i-4), i=1,4)], .true., [dt(4):: (dt(4)(i+4), i=1,9)])
  if (any(array /= [dt(4):: dt(4)(-3), dt(4)(-2), dt(4)(-1), dt(4)(0), dt(4)(9), dt(4)(10), dt(4)(11), dt(4)(12), dt(4)(13)])) stop 16


  ! SPREAD
  array = spread(dt(4)(1), 1, 9)
  if (any(array /= dt(4)(1))) stop 17

  array = [dt(4):: spread(dt(4)(1), 1, 9)]
  if (any(array /= dt(4)(1))) stop 18

  print *, spread([dt(4):: dt(4)(1),dt(4)(2),dt(4)(3)], 1, 3)
  array = [dt(4):: spread([dt(4):: dt(4)(1),dt(4)(2),dt(4)(3)], 1, 3)]
  if (any(array /= [dt(4):: (dt(4)(i),dt(4)(i),dt(4)(i),i=1,3)])) stop 19

  print *, spread([dt(4):: (dt(4)(i), i=1,3)], 1, 3)
  array = [dt(4):: spread([dt(4):: (dt(4)(i), i=1,3)], 1, 3)]
  if (any(array /= [dt(4):: (dt(4)(i),dt(4)(i),dt(4)(i),i=1,3)])) stop 20


  ! TRANSPOSE
  array19 = reshape([dt(4):: (dt(4)(i), i=1,9)], [1,9])
  array91 = transpose (array19)
  if (any([dt(4):: array19] /= [dt(4):: array91])) stop 21

  array33 = reshape([dt(4):: (dt(4)(i), i=1,9)], [3,3])
  array33 = transpose(array33)
  if (any([dt(4):: array33] /= [dt(4):: dt(4)(1),dt(4)(4),dt(4)(7),dt(4)(2),dt(4)(5),dt(4)(8),dt(4)(3),dt(4)(6),dt(4)(9)])) stop 22
  print *, array33
  array33 = transpose(reshape([dt(4):: (dt(4)(i), i=1,9)], [3,3]))
  if (any([dt(4):: array33] /= [dt(4):: dt(4)(1),dt(4)(4),dt(4)(7),dt(4)(2),dt(4)(5),dt(4)(8),dt(4)(3),dt(4)(6),dt(4)(9)])) stop 23
  if (any([dt(4):: transpose(reshape([dt(4):: (dt(4)(i), i=1,9)], [3,3]))] /= [dt(4):: dt(4)(1),dt(4)(4),dt(4)(7),dt(4)(2),dt(4)(5),dt(4)(8),dt(4)(3),dt(4)(6),dt(4)(9)])) stop 24

  array91 = transpose(reshape([dt(4):: (dt(4)(i), i=1,9)], [1,9]))
  print *, array91
  print *, transpose(reshape([dt(4):: (dt(4)(i), i=1,9)], [1,9]))

  if (any([dt(4):: transpose(reshape([dt(4):: dt(4)(1),dt(4)(4),dt(4)(7),dt(4)(2),dt(4)(5),dt(4)(8),dt(4)(3),dt(4)(6),dt(4)(9)], [3,3]))] /= [dt(4):: (dt(4)(i), i=1,9)])) stop 25

  if (any([dt(4):: transpose(transpose(reshape([dt(4):: (dt(4)(i), i=1,9)], [3,3])))] /= [dt(4):: (dt(4)(i), i=1,9)])) stop 26

  if (any([dt(4):: transpose(transpose(reshape([dt(4):: (dt(4)(i), i=1,1024)], [8,128])))] /= [dt(4):: (dt(4)(i), i=1,1024)])) stop 27


  ! UNPACK
  mask = reshape([logical:: T, T, F, F, T, T, F, F, T], [integer:: 3,3])
  ! T . .
  ! T T .
  ! . T T

  array = [dt(4):: (dt(4)(i), i=1,9)]
  print *, unpack(array, mask, dt(4)(999)) ! 1 2 999 999 3 4 999 999 5
  array = [dt(4):: unpack(array, mask, dt(4)(999))]
  if (any(array /= [dt(4):: dt(4)(1), dt(4)(2), dt(4)(999), dt(4)(999), dt(4)(3), dt(4)(4), dt(4)(999), dt(4)(999), dt(4)(5)])) stop 28

  print *, unpack([dt(4):: dt(4)(1),dt(4)(2),dt(4)(3),dt(4)(4),dt(4)(5),dt(4)(6),dt(4)(7),dt(4)(8),dt(4)(9)], [logical:: T, T, F, F, T, T, F, F, T], dt(4)(999))
  array = unpack([dt(4):: dt(4)(1),dt(4)(2),dt(4)(3),dt(4)(4),dt(4)(5),dt(4)(6),dt(4)(7),dt(4)(8),dt(4)(9)], [logical:: T, T, F, F, T, T, F, F, T], dt(4)(999))
  if (any(array /= [dt(4):: dt(4)(1), dt(4)(2), dt(4)(999), dt(4)(999), dt(4)(3), dt(4)(4), dt(4)(999), dt(4)(999), dt(4)(5)])) stop 29

  array = [dt(4):: unpack([dt(4):: dt(4)(1),dt(4)(2),dt(4)(3),dt(4)(4),dt(4)(5),dt(4)(6),dt(4)(7),dt(4)(8),dt(4)(9)], [logical:: T, T, F, F, T, T, F, F, T], dt(4)(999))]
  if (any(array /= [dt(4):: dt(4)(1), dt(4)(2), dt(4)(999), dt(4)(999), dt(4)(3), dt(4)(4), dt(4)(999), dt(4)(999), dt(4)(5)])) stop 30

  print *, unpack([dt(4):: dt(4)(1),dt(4)(2),dt(4)(3),dt(4)(4),dt(4)(5),dt(4)(6),dt(4)(7),dt(4)(8),dt(4)(9)], reshape([logical:: T, T, F, F, T, T, F, F, T], [integer:: 3,3]), dt(4)(999))
  array = [dt(4):: unpack([dt(4):: dt(4)(1),dt(4)(2),dt(4)(3),dt(4)(4),dt(4)(5),dt(4)(6),dt(4)(7),dt(4)(8),dt(4)(9)], reshape([logical:: T, T, F, F, T, T, F, F, T], [integer:: 3,3]), dt(4)(999))]
  if (any(array /= [dt(4):: dt(4)(1), dt(4)(2), dt(4)(999), dt(4)(999), dt(4)(3), dt(4)(4), dt(4)(999), dt(4)(999), dt(4)(5)])) stop 31

  array = unpack([dt(4):: dt(4)(1),dt(4)(2),dt(4)(3),dt(4)(4),dt(4)(5),dt(4)(6),dt(4)(7),dt(4)(8),dt(4)(9)], [logical:: T, T, F, F, T, T, F, F, T], [dt(4):: dt(4)(81), dt(4)(64), dt(4)(49), dt(4)(36), dt(4)(25), dt(4)(16), dt(4)(9), dt(4)(4), dt(4)(1)])
  if (any(array /= [dt(4):: dt(4)(1), dt(4)(2), dt(4)(49), dt(4)(36), dt(4)(3), dt(4)(4), dt(4)(9), dt(4)(4), dt(4)(5)])) stop 32

  array33 = unpack([dt(4):: dt(4)(1),dt(4)(2),dt(4)(3),dt(4)(4),dt(4)(5)], reshape([logical:: T, T, F, F, T, T, F, F, T], [3,3]), reshape([dt(4):: dt(4)(81), dt(4)(64), dt(4)(49), dt(4)(36), dt(4)(25), dt(4)(16), dt(4)(9), dt(4)(4), dt(4)(1)],[3,3]))
  if (any([dt(4):: array33] /= [dt(4):: dt(4)(1), dt(4)(2), dt(4)(49), dt(4)(36), dt(4)(3), dt(4)(4), dt(4)(9), dt(4)(4), dt(4)(5)])) stop 33

end program acetdt60ext
