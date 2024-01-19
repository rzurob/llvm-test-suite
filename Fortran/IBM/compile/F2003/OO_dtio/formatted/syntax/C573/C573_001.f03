! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: C573
!*                                        A namelist group name shall not be a name made accessible by use association
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type base
      real(4), allocatable :: i
      real(4), pointer     :: j
   end type

   class(base), allocatable :: b1
   namelist /nml/ b1

end module

program C573_001
   use m

   class(base), pointer     :: b2
   type(base)               :: b3

   namelist /nml/ b2, b3

   integer :: stat
   character(200) :: msg

   allocate(b1, b1%i, b1%j)
   allocate(b2, b2%i, b2%j)
   allocate(b3%i, b3%j)

end program
