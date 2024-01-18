!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2006-11-24
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : array construction intrinsics (derived type)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Use array construction functions not used elsewhere.  We're already using
!*  RESHAPE in many places; MERGE has also been used, but not in its array
!*  form (e.g., merge(1,2,log) is 1 if log is true, and otherwise 2).  We
!*  have not used these yet:
!*     CSHIFT    (array, shift [,dim])           Circular shift
!*     EOSHIFT   (array, shift [,boundary, dim]) End-off shift
!*     MERGE     (tsource, fsource, mask)        Merge under mask
!*     PACK      (array, mask [,vector])         Pack into an array of rank one under a mask
!*     SPREAD    (source, dim, ncopies)          Replicates array by adding a dimension
!*     TRANSPOSE (matrix)                        Transpose a rank-2 array
!*     UNPACK    (vector, mask, field)           inverse of pack
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetdt60mod

  implicit none
  type dt
     integer :: ival
   contains
     procedure :: isNotEqual
     generic :: operator(.ne.) => isNotEqual
     procedure :: div
     generic :: operator(/) => div
  end type dt

contains

  elemental logical function isNotEqual(this, that)
    class (dt), intent(in) :: this, that
    isNotEqual = this % ival /= that % ival
  end function isNotEqual

  elemental integer function div(this, that)
    class (dt), intent(in) :: this
    integer, intent(in) :: that
    div = this % ival / that
  end function div

end module acetdt60mod


program acetdt60

  use acetdt60mod
  implicit none
  type (dt) :: array(9), array19(1,9), array91(9,1), array33(3,3)
  logical, parameter :: T = .true., F = .false.
  logical :: mask (3,3)
  integer :: i


  ! Circular shift
  if (any(cshift ([dt:: (dt(i),i=1,9)], 2) /= [dt:: (dt(i),i=3,9), dt(1), dt(2)])) stop 2
  array = cshift ([dt:: (dt(i ** 2), i=1,9)], -1)
  if (any(array /= [dt:: dt(9**2), (dt(i ** 2), i=1,8)])) stop 3
  print *, cshift ([dt:: (dt(i), i=9,1,-1)], -2)

  array = [dt:: (dt(i), i=1,9)]
  array = [dt:: cshift(array,shift=3)]
  if (any(array /= [dt:: (dt(i), i=4,9), (dt(i), i=1,3)])) stop 4
  array = [dt:: (dt(i), i=1,9)]
  array = [dt:: cshift(reshape(array,[integer:: 3,3]), shift=[integer:: -1, 1, 0], dim=2)]
  if (any(array /= [dt:: dt(7),dt(5),dt(3), dt(1),dt(8),dt(6), dt(4),dt(2),dt(9)])) stop 5

  array = [dt:: (dt(i), i=1,9)]
  array = [dt:: cshift(reshape(array,[integer:: 3,3]), shift=[integer:: array(1:8:3)%ival], dim=2)]
  if (any(array /= [dt:: dt(4),dt(5),dt(6), dt(7),dt(8),dt(9), dt(1),dt(2),dt(3)])) stop 6


  ! End-off shift
  if (any(eoshift ([dt:: (dt(i),i=1,9)], 2, dt(0)) /= [dt:: (dt(i),i=3,9), dt(0), dt(0)])) stop 7
  array = eoshift ([dt:: (dt(i ** 2), i=1,9)], -1, dt(12))
  if (any(array /= [dt:: dt(12), (dt(i ** 2), i=1,8)])) stop 8
  print *, eoshift ([dt:: (dt(i), i=9,1,-1)], -2, dt(99))

  array = [dt:: (dt(i), i=1,9)]
  array = [dt:: eoshift(array,shift=3,boundary=dt(0))]
  if (any(array /= [dt:: (dt(i), i=4,9), (dt(0), i=1,3)])) stop 9
  array = [dt:: (dt(i), i=1,9)]
  array = [dt:: eoshift(reshape(array,[integer:: 3,3]), shift=[integer:: -1, 1, 0], boundary = [dt:: array(1:8:3)], dim=2)]
  if (any(array /= [dt:: dt(1),dt(5),dt(3), dt(1),dt(8),dt(6), dt(4),dt(4),dt(9)])) stop 10

  array = [dt:: (dt(i), i=1,9)]
  array = [dt:: eoshift(reshape(array,[integer:: 3,3]), shift=[integer:: array(1:8:3)%ival], boundary = [dt:: array(1:3)], dim=2)]
  if (any(array /= [dt:: dt(4),dt(2),dt(3), dt(7),dt(2),dt(3), dt(1),dt(2),dt(3)])) stop 11


  ! merge
  print *, merge([dt:: (dt(i), i=1,5)], [dt:: (dt(i ** 2), i=1,5)], [logical:: T, F, T, T, F]) ! 1 4 3 4 25
  if (any(merge([dt:: (dt(i), i=1,5)], [dt:: (dt(i ** 2), i=1,5)], [logical:: T, F, T, T, F]) /= [dt:: dt(1), dt(4), dt(3), dt(4), dt(25)])) stop 12
  array = [dt:: (dt(i), i=1,9)]
  array = merge([dt:: (array(i), i=1,9)], [dt:: (dt(array(i)%ival ** 2), i=9,1,-1)], [logical:: T, F, T, T, F, F, F, T, T])
  if (any(array /= [dt:: dt(1), dt(64), dt(3), dt(4), dt(25), dt(16), dt(9), dt(8), dt(9)])) stop 13


  ! PACK
  array = [dt:: (dt(i), i=1,9)]
  print *, pack(reshape([dt:: (dt(i ** 2), i=1,9)], [integer:: 3,3]), &
                mod(reshape(array%ival,[3,3]),3) == 0) ! 9 36 81
  array(1:3) = pack(reshape([dt:: (dt(i ** 2), i=1,9)], [integer:: 3,3]), &
                    mod(reshape(array%ival,[3,3]),3) == 0)
  if (any(array /= [dt:: dt(9), dt(36), dt(81), dt(4), dt(5), dt(6), dt(7), dt(8), dt(9)])) stop 14

  print *, pack(reshape([dt:: (dt(i ** 2), i=1,9)], [integer:: 3,3]), &
                mod(reshape(array%ival,[3,3]),3) == 0, [dt:: (dt(i ** 3), i=9,1,-1)]) ! 1 4 9 36 81 64 27 8 1
  array =  pack(reshape([dt:: (dt(i ** 2), i=1,9)], [integer:: 3,3]), &
                mod(reshape(array%ival,[3,3]),3) == 0, [dt:: (dt(i ** 3), i=9,1,-1)])
  if (any(array /= [dt:: dt(1), dt(4), dt(9), dt(36), dt(81), dt(64), dt(27), dt(8), dt(1)])) stop 15

  print *, pack([dt:: (dt(i-4), i=1,4)], .true., [dt:: (dt(i+4), i=1,9)])
  array = pack([dt:: (dt(i-4), i=1,4)], .true., [dt:: (dt(i+4), i=1,9)])
  if (any(array /= [dt:: dt(-3), dt(-2), dt(-1), dt(0), dt(9), dt(10), dt(11), dt(12), dt(13)])) stop 16


  ! SPREAD
  array = spread(dt(1), 1, 9)
  if (any(array /= dt(1))) stop 17

  array = [dt:: spread(dt(1), 1, 9)]
  if (any(array /= dt(1))) stop 18

  print *, spread([dt:: dt(1),dt(2),dt(3)], 1, 3)
  array = [dt:: spread([dt:: dt(1),dt(2),dt(3)], 1, 3)]
  if (any(array /= [dt:: (dt(i),dt(i),dt(i),i=1,3)])) stop 19

  print *, spread([dt:: (dt(i), i=1,3)], 1, 3)
  array = [dt:: spread([dt:: (dt(i), i=1,3)], 1, 3)]
  if (any(array /= [dt:: (dt(i),dt(i),dt(i),i=1,3)])) stop 20


  ! TRANSPOSE
  array19 = reshape([dt:: (dt(i), i=1,9)], [1,9])
  array91 = transpose (array19)
  if (any([dt:: array19] /= [dt:: array91])) stop 21

  array33 = reshape([dt:: (dt(i), i=1,9)], [3,3])
  array33 = transpose(array33)
  if (any([dt:: array33] /= [dt:: dt(1),dt(4),dt(7),dt(2),dt(5),dt(8),dt(3),dt(6),dt(9)])) stop 22
  print *, array33
  array33 = transpose(reshape([dt:: (dt(i), i=1,9)], [3,3]))
  if (any([dt:: array33] /= [dt:: dt(1),dt(4),dt(7),dt(2),dt(5),dt(8),dt(3),dt(6),dt(9)])) stop 23
  if (any([dt:: transpose(reshape([dt:: (dt(i), i=1,9)], [3,3]))] /= [dt:: dt(1),dt(4),dt(7),dt(2),dt(5),dt(8),dt(3),dt(6),dt(9)])) stop 24

  array91 = transpose(reshape([dt:: (dt(i), i=1,9)], [1,9]))
  print *, array91
  print *, transpose(reshape([dt:: (dt(i), i=1,9)], [1,9]))

  if (any([dt:: transpose(reshape([dt:: dt(1),dt(4),dt(7),dt(2),dt(5),dt(8),dt(3),dt(6),dt(9)], [3,3]))] /= [dt:: (dt(i), i=1,9)])) stop 25

  if (any([dt:: transpose(transpose(reshape([dt:: (dt(i), i=1,9)], [3,3])))] /= [dt:: (dt(i), i=1,9)])) stop 26

  if (any([dt:: transpose(transpose(reshape([dt:: (dt(i), i=1,1024)], [8,128])))] /= [dt:: (dt(i), i=1,1024)])) stop 27


  ! UNPACK
  mask = reshape([logical:: T, T, F, F, T, T, F, F, T], [integer:: 3,3])
  ! T . .
  ! T T .
  ! . T T

  array = [dt:: (dt(i), i=1,9)]
  print *, unpack(array, mask, dt(999)) ! 1 2 999 999 3 4 999 999 5
  array = [dt:: unpack(array, mask, dt(999))]
  if (any(array /= [dt:: dt(1), dt(2), dt(999), dt(999), dt(3), dt(4), dt(999), dt(999), dt(5)])) stop 28

  print *, unpack([dt:: dt(1),dt(2),dt(3),dt(4),dt(5),dt(6),dt(7),dt(8),dt(9)], [logical:: T, T, F, F, T, T, F, F, T], dt(999))
  array = unpack([dt:: dt(1),dt(2),dt(3),dt(4),dt(5),dt(6),dt(7),dt(8),dt(9)], [logical:: T, T, F, F, T, T, F, F, T], dt(999))
  if (any(array /= [dt:: dt(1), dt(2), dt(999), dt(999), dt(3), dt(4), dt(999), dt(999), dt(5)])) stop 29

  array = [dt:: unpack([dt:: dt(1),dt(2),dt(3),dt(4),dt(5),dt(6),dt(7),dt(8),dt(9)], [logical:: T, T, F, F, T, T, F, F, T], dt(999))]
  if (any(array /= [dt:: dt(1), dt(2), dt(999), dt(999), dt(3), dt(4), dt(999), dt(999), dt(5)])) stop 30

  print *, unpack([dt:: dt(1),dt(2),dt(3),dt(4),dt(5),dt(6),dt(7),dt(8),dt(9)], reshape([logical:: T, T, F, F, T, T, F, F, T], [integer:: 3,3]), dt(999))
  array = [dt:: unpack([dt:: dt(1),dt(2),dt(3),dt(4),dt(5),dt(6),dt(7),dt(8),dt(9)], reshape([logical:: T, T, F, F, T, T, F, F, T], [integer:: 3,3]), dt(999))]
  if (any(array /= [dt:: dt(1), dt(2), dt(999), dt(999), dt(3), dt(4), dt(999), dt(999), dt(5)])) stop 31

  array = unpack([dt:: dt(1),dt(2),dt(3),dt(4),dt(5),dt(6),dt(7),dt(8),dt(9)], [logical:: T, T, F, F, T, T, F, F, T], [dt:: dt(81), dt(64), dt(49), dt(36), dt(25), dt(16), dt(9), dt(4), dt(1)])
  if (any(array /= [dt:: dt(1), dt(2), dt(49), dt(36), dt(3), dt(4), dt(9), dt(4), dt(5)])) stop 32

  array33 = unpack([dt:: dt(1),dt(2),dt(3),dt(4),dt(5)], reshape([logical:: T, T, F, F, T, T, F, F, T], [3,3]), reshape([dt:: dt(81), dt(64), dt(49), dt(36), dt(25), dt(16), dt(9), dt(4), dt(1)],[3,3]))
  if (any([dt:: array33] /= [dt:: dt(1), dt(2), dt(49), dt(36), dt(3), dt(4), dt(9), dt(4), dt(5)])) stop 33

end program acetdt60
