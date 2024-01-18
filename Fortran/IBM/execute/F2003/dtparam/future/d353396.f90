!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/19/2009
!*
!*  DESCRIPTION                : test case to track defect 353396
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type base(l)
      integer,len:: l
      integer(l%kind) :: a
   end type

   type base2(l)
      integer,len:: l
      integer(kind(l)) :: a
   end type

   type base3(k)
       integer, kind :: k
       integer(k) :: i = i%kind
   end type
end module

  use m
  implicit none

  ! more to test if the bug of 353396 is fully fixed
  end

