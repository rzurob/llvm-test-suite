!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 13, 2012
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop: Assumed Type objects
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : TYPE(*) cannot appear in type-spec of
!*                               an ALLOCATE statement
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
program AssumedType08d
implicit none
class(*), allocatable :: i
class(*), allocatable :: arr(:)
class(*), pointer     :: p

allocate( TYPE(*) :: i)
allocate( TYPE(*) :: p)

allocate( * :: i)
allocate( * :: arr(5))
allocate( * :: p)

end program AssumedType08d
